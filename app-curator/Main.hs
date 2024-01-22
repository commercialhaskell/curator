{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Curator hiding (Snapshot)
import Data.Yaml (encodeFile, decodeFileThrow)
import Network.HTTP.Client (httpLbs, newManager, parseUrlThrow, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Download (download)
import Options.Applicative.Simple hiding (action)
import qualified Pantry
import Path ((</>), addExtension, toFilePath, Path, Abs, File)
import Path.IO (doesFileExist, removeFile, resolveFile', resolveDir')
import Paths_curator (version)
import qualified RIO.ByteString.Lazy as BL
import RIO.List (stripPrefix)
import qualified RIO.Map as Map
import RIO.Process
import qualified RIO.Text as T
import RIO.Time

options :: IO ((), RIO PantryApp ())
options =
    simpleOptions $(simpleVersion version)
                  "curator - Stackage curator tool"
                  "Special utilities for Stackage curators"
                  (pure ())
                  commands
  where
    commands = do
      addCommand "update"
                 "Update Pantry databse from Hackage"
                 (const update)
                 (pure ())
      addCommand "check-target-available"
                 "Check if target snapshot isn't yet on Github"
                 checkTargetAvailable
                 parseTarget
      addCommand "constraints"
                 "Generate constraints file from build-constraints.yaml"
                 constraints
                 parseConstraintsArgs
      addCommand "snapshot-incomplete"
                 "Generate incomplete snapshot"
                 snapshotIncomplete
                 parseTarget
      addCommand "snapshot"
                 "Complete locations in incomplete snapshot"
                 (const snapshot)
                 (pure ())
      addCommand "check-snapshot"
                 "Check snapshot consistency"
                 (const checkSnapshot)
                 (pure ())
      addCommand "legacy-snapshot"
                 "Generate a legacy-format snapshot file"
                 (const legacySnapshot)
                 (pure ())
      addCommand "unpack"
                 "Unpack snapshot packages and create a Stack project for it"
                 (const unpackFiles)
                 (pure ())
      addCommand "build"
                 "Build Stack project for a Stackage snapshot"
                 build
                 parseJobs
      addCommand "upload-docs"
                 "Upload documentation to an S3 bucket"
                 uploadDocs'
                 parseUploadDocsArgs
      addCommand "upload-github"
                 "Commit and push snapshot definition to Github repository"
                 uploadGithub'
                 parseTarget
      addCommand "hackage-distro"
                 "Upload list of snapshot packages on Hackage as a distro"
                 hackageDistro
                 parseTarget
      addCommand "legacy-bulk"
                 "Bulk convert all new snapshots to the legacy LTS/Nightly directories"
                 legacyBulk
                 parseLegacyBulkArgs

    parseConstraintsArgs = ConstraintsArgs
      <$> flag True False (long "no-download" <> help "Download constraints file for LTS")
      <*> parseTarget
    parseTarget =
      option (nightly <|> lts) ( long "target"
                              <> metavar "TARGET"
                              <> help "Target Stackage snapshot 'lts-MM.NN' or 'nightly-YYYY-MM-DD'"
                               )
    parseUploadDocsArgs = UploadArgs <$> parseTarget <*>
        strOption (mconcat
            [ long "bucket"
            , metavar "BUCKET"
            , value "haddock.stackage.org"
            , showDefault
            , help "Target bucket name. All other AWS params are taken from environment variables."
            ])
    nightly = maybeReader $ \s -> do
      s' <- stripPrefix "nightly-" s
      TargetNightly <$> parseTimeM False defaultTimeLocale "%Y-%m-%d" s'
    lts = maybeReader $ \s -> do
      s' <- stripPrefix "lts-" s
      case break (== '.') s' of
        (major, '.':minor) -> TargetLts <$> readMaybe major <*> readMaybe minor
        _ -> Nothing
    parseJobs = option auto ( long "jobs"
                           <> metavar "JOBS"
                           <> showDefault
                           <> value 1
                           <> help "Number of jobs to run Stackage build with"
                              )
    parseLegacyBulkArgs = LegacyBulkArgs
      <$> strOption (long "stackage-snapshots" <> metavar "DIR")
      <*> strOption (long "lts-haskell" <> metavar "DIR")
      <*> strOption (long "stackage-nightly" <> metavar "DIR")

data UploadArgs = UploadArgs Target Text

main :: IO ()
main = runPantryApp $ do
  ((), runCmd) <- liftIO options
  runCmd

update :: RIO PantryApp ()
update = do
  void $ updateHackageIndex $ Just "Updating hackage index"

data ConstraintsArgs = ConstraintsArgs
  { downloadLtsConstraints :: Bool
  , target :: Target
  }

constraints :: ConstraintsArgs -> RIO PantryApp ()
constraints ConstraintsArgs { downloadLtsConstraints, target } = do
  stackageConstraints <- case target of
    TargetNightly _ -> nightlyConstraints
    TargetLts major minor -> ltsConstraints downloadLtsConstraints major minor
  logInfo "Writing constraints.yaml"
  liftIO $ encodeFile constraintsFilename stackageConstraints

nightlyConstraints = do
  buildConstraintsPath <- resolveFile' "build-constraints.yaml"
  exists <- doesFileExist buildConstraintsPath
  if exists
    then do
      logInfo $ "Reusing already existing file " <> fromString (toFilePath buildConstraintsPath)
      loadStackageConstraints $ toFilePath buildConstraintsPath
    else do
      logInfo $ "Downloading " <> fromString (toFilePath buildConstraintsPath)
        <> " from commercialhaskell/stackage"
      req <- parseUrlThrow $ "https://raw.githubusercontent.com/commercialhaskell/stackage/master/build-constraints.yaml"
      man <- liftIO $ newManager tlsManagerSettings
      liftIO (httpLbs req man) >>= loadStackageConstraintsBs . BL.toStrict . responseBody

ltsConstraints download major minor = do
  when (minor > 0) $ do
    verifyPreviousLtsMinorExists major minor
  let buildConstraintsName = "lts-" <> show major <> "-build-constraints.yaml"
  buildConstraintsPath <- resolveFile' buildConstraintsName
  exists <- doesFileExist buildConstraintsPath
  if download
  then do
    logInfo $ "Downloading " <> fromString (buildConstraintsName) <> " from commercialhaskell/lts-haskell"
    req <- parseUrlThrow $ "https://raw.githubusercontent.com/commercialhaskell/lts-haskell/master/build-constraints/" <> buildConstraintsName
    man <- liftIO $ newManager tlsManagerSettings
    liftIO (httpLbs req man) >>= loadStackageConstraintsBs . BL.toStrict . responseBody
  else do
      logInfo $ "Reusing local file " <> fromString (toFilePath buildConstraintsPath)
      loadStackageConstraints $ toFilePath buildConstraintsPath

-- Performs a download of the previous LTS minor just to verify that it has been published.
verifyPreviousLtsMinorExists :: Int -> Int -> RIO PantryApp ()
verifyPreviousLtsMinorExists major minor = do
  let prevMinor = minor - 1
      url = concat
        [ "https://raw.githubusercontent.com/" ++ constraintsRepo ++ "/master/lts/"
        , show major
        , "/"
        , show prevMinor
        , ".yaml"
        ]
  -- Hacky, use a presumably unique file name for this download (the file is not used).
  constraintsPath <- addExtension ".previous" =<< resolveFile' constraintsFilename
  exists <- doesFileExist constraintsPath
  when exists $
    removeFile constraintsPath
  logInfo $ "Verifying existence of constraints.yaml from "
    <> "lts-" <> display major <> "." <> display prevMinor
  req <- parseUrlThrow url
  downloaded <- download req constraintsPath
  unless downloaded $
    error $ "Could not download constraints.yaml from " <> url

snapshotIncomplete :: Target -> RIO PantryApp ()
snapshotIncomplete target = do
  logInfo "Writing snapshot-incomplete.yaml"
  decodeFileThrow constraintsFilename >>= \constraintsNoLts -> do
    constraints' <-
      case target of
        TargetLts major minor
          | minor > 0 -> applyPvpRules constraintsNoLts major (minor - 1)
        _ -> pure constraintsNoLts
    snapshot' <- makeSnapshot constraints'
    liftIO $ encodeFile "snapshot-incomplete.yaml" snapshot'

snapshot :: RIO PantryApp ()
snapshot = do
  logInfo "Writing snapshot.yaml"
  incomplete <- loadPantrySnapshotLayerFile "snapshot-incomplete.yaml"
  complete <- completeSnapshotLayer incomplete
  liftIO $ encodeFile snapshotFilename complete

completeSnapshotLayer
  :: RawSnapshotLayer
  -> RIO PantryApp SnapshotLayer
completeSnapshotLayer RawSnapshotLayer {..} = do
  slParent <- completeSnapshotLocation rslParent
  slLocations <- for rslLocations $ \rawLoc -> do
    cpl <- completePackageLocation rawLoc
    if cplHasCabalFile cpl
      then pure $ cplComplete cpl
      else throwString $ "We're no longer supporting packages without cabal files: " ++ show rawLoc
  let slCompiler = rslCompiler
      slDropPackages = rslDropPackages
      slFlags = rslFlags
      slHidden = rslHidden
      slGhcOptions = rslGhcOptions
      slPublishTime = rslPublishTime
  pure SnapshotLayer {..}

loadSnapshotYaml :: RIO PantryApp Pantry.Snapshot
loadSnapshotYaml = do
  abs' <- resolveFile' snapshotFilename
  let sloc = SLFilePath $
        ResolvedPath (RelFilePath (fromString snapshotFilename)) abs'
  (snap, _, _) <- loadAndCompleteSnapshot sloc Map.empty Map.empty
  pure snap

checkSnapshot :: RIO PantryApp ()
checkSnapshot = do
  logInfo "Checking dependencies in snapshot.yaml"
  decodeFileThrow constraintsFilename >>= \constraints' -> do
    snapshot' <- loadSnapshotYaml
    checkDependencyGraph constraints' snapshot'

legacySnapshot :: RIO PantryApp ()
legacySnapshot = do
  logInfo "Generating legacy-style snapshot file in legacy-snapshot.yaml"
  snapshot' <- loadSnapshotYaml
  legacy <- toLegacySnapshot snapshot'
  liftIO $ encodeFile "legacy-snapshot.yaml" legacy

unpackDir :: FilePath
unpackDir = "unpack-dir"

unpackFiles :: RIO PantryApp ()
unpackFiles = do
  logInfo "Unpacking files"
  snapshot' <- loadSnapshotYaml
  constraints' <- decodeFileThrow constraintsFilename
  dest <- resolveDir' unpackDir
  unpackSnapshot constraints' snapshot' dest

build :: Int -> RIO PantryApp ()
build jobs = do
  logInfo "Building"
  withWorkingDir unpackDir $ proc
    "stack"
    ["--terminal",
     "build",
     "--test", "--test-suite-timeout=600", "--no-rerun-tests",
     "--bench", "--no-run-benchmarks",
     "--haddock",
     "--no-interleaved-output",
     "--ghc-options", "-w",
     "--jobs=" ++ show jobs
    ]
    runProcess_

hackageDistro :: Target -> RIO PantryApp ()
hackageDistro target = do
  logInfo "Uploading Hackage distro for snapshot.yaml"
  snapshot' <- loadSnapshotYaml
  let packageVersions =
        Map.mapMaybe (snapshotVersion . spLocation) (snapshotPackages snapshot')
  uploadHackageDistro target packageVersions

uploadDocs' :: UploadArgs -> RIO PantryApp ()
uploadDocs' (UploadArgs target bucket) = do
  docsDir <- fmap (T.unpack . T.dropSuffix "\n" . decodeUtf8Lenient . BL.toStrict) $
    withWorkingDir unpackDir $ proc "stack" (words "path --local-doc-root") readProcessStdout_
  logInfo "Uploading docs to S3"
  let prefix = utf8BuilderToText $
        case target of
          TargetNightly day ->
            let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing) day
            in "nightly-" <> fromString date
          TargetLts x y ->
            "lts-" <> display x <> "." <> display y
  uploadDocs docsDir prefix bucket

uploadGithub' :: Target -> RIO PantryApp ()
uploadGithub' target = do
  logInfo "Uploading snapshot definition to Github"
  uploadGithub target

loadPantrySnapshotLayerFile :: FilePath -> RIO PantryApp RawSnapshotLayer
loadPantrySnapshotLayerFile fp = do
  abs' <- resolveFile' fp
  eres <- loadSnapshotLayer $ SLFilePath (ResolvedPath (RelFilePath (fromString fp)) abs')
  case eres of
    Left x -> error $ "should not happen: " ++ show (fp, x)
    Right x -> pure x
