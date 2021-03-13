{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- | Deal with the @build-constraints.yaml@ format used by
-- @commercialhaskell/stackage@.
module Curator.StackageConstraintsV2
  ( loadStackageConstraints
  , loadStackageConstraintsBs
  ) where

import Data.Maybe
import Pantry
import Curator.Types
import Data.Aeson
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Distribution.Types.VersionRange (VersionRange, intersectVersionRanges)
import Distribution.Types.Flag (mkFlagName)
import Data.Yaml
import Distribution.Text (simpleParse)
import qualified Curator.StackageConstraints as V1

data SCPackage = SCPackage
  { maintainers :: Set Maintainer
  , versionRanges :: [VersionRange] -- TODO change to VersionRange
  , flags :: Map FlagName Bool
  -- ^ Packages to include
  , skippedBuild :: Bool
  -- ^ Include the package in the snapshot, but don't build
  -- it. Intended for Windows-specific packages.
  , tests :: RunConfig
  -- ^ Allow configuring whether to compile+run the test suite, only compile it, or do nothing with it.
  , benchmarks :: RunConfig
  -- ^ Like 'tests'
  , haddock :: RunConfig
  -- ^ Like 'tests'
  , tellMeWhenItsReleased :: Maybe Version
  -- ^ Fail the build when a newer version than this has been released
  , hide :: Bool
  -- ^ (???) Don't expose in package DB
  , noRevisions :: Bool
  -- ^ Ignore hackage revisions
  , nonParallelBuild :: Bool
  -- ^ Expensive, so don't build it at the same time as other packages.
  , tarballUrl :: Maybe Text
  -- ^ (???) Link to the release tarball, used instead of hackage
  }
  deriving Show

scPackageToTuple :: SCPackage -> (Set Maintainer, [VersionRange])
scPackageToTuple SCPackage{..} = (maintainers, versionRanges)

data SC = SC
  { scGhcVersion :: !Version
  -- ^ GHC version to use

  , scPackages :: !(Map PackageName SCPackage)

  , scGithubUsers :: !(Map Text (Set Text))
  -- ^ Mapping from Github org to users who will receive pings

  -- TODO: Lots of stuff was moved from V1's SC into SCPackage, may
  -- want to keep them for e.g. GHC upgrades.
  }
  deriving Show

instance FromJSON SC where
  parseJSON = withObject "StackageConstraints" $ \o -> do
    CabalString scGhcVersion <- o .: "ghc-version"
    scPackages <- convertPackages <$> o .: "packages"
    scGithubUsers <- o .: "github-users"
    pure SC {..}

data MaintainerDefinition = MaintainerDefinition
  { maintainerPackages :: [MaintainerPackage]
  }

newtype SCVersionRange = SCVersionRange { unSCVersionRange :: VersionRange }
  deriving stock (Eq, Generic, Show)

instance FromJSON SCVersionRange where
  parseJSON = withText "SCVersionRange" $ \t -> do
    let s = T.unpack t
    maybe (fail $ "Invalid SCVersionRange: " ++ s) pure $ do
      let (nameT, T.strip -> rangeT) = T.break (== ' ') t
      if T.null rangeT
      then Nothing
      else do
        name <- simpleParse $ T.unpack rangeT
        pure $ SCVersionRange name

newtype SCVersion = SCVersion { unSCVersion :: Version }
  deriving stock (Eq, Generic, Show)

instance FromJSON SCVersion where
  parseJSON = withText "SCVersion" $ \t ->
    case readMaybe (T.unpack t) of
      Nothing -> fail $ "Invalid SCVersion: " <> T.unpack t
      Just v -> pure $ SCVersion v

newtype SCPackageName = SCPackageName { unSCPackageName :: PackageName }
  deriving stock (Eq, Generic, Show)

instance FromJSON SCPackageName where
  parseJSON = withText "SCPackageName" $ \t -> do
    let s = T.unpack t
    maybe (fail $ "Invalid SCPackageName: " ++ s) pure $ do
      let (nameT, T.strip -> rangeT) = T.break (== ' ') t
      name <- simpleParse $ T.unpack nameT
      pure $ SCPackageName name

newtype SCFlagName = SCFlagName { unSCFlagName :: FlagName }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSONKey SCFlagName where
instance FromJSON SCFlagName where
  parseJSON = fmap (SCFlagName . mkFlagName) . parseJSON

data MaintainerPackage = MaintainerPackage
  { mpName                  :: SCPackageName
  , mpRange                 :: Maybe SCVersionRange
  , mpFlags                 :: Maybe (Map SCFlagName Bool)
  , mpSkippedBuild          :: Maybe Bool
  , mpTests                 :: Maybe RunConfig
  , mpBenchmarks            :: Maybe RunConfig
  , mpHaddock               :: Maybe RunConfig
  , mpTellMeWhenItsReleased :: Maybe SCVersion
  , mpHide                  :: Maybe Bool
  , mpNoRevisions           :: Maybe Bool
  , mpNonParallelBuild      :: Maybe Bool
  , mpTarballUrl            :: Maybe Text
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)

data RunConfig
  = ExpectSuccess
  -- ^ Expected work. Error out if they fail.
  | ExpectFailure
  -- ^ Run, but don't error out on failures.
  | Skip
  -- ^ Don't even try, for out-of-bounds dependencies
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

convertRunConfig = \case
  ExpectSuccess -> CAExpectSuccess
  ExpectFailure -> CAExpectFailure
  Skip -> CASkip

convertPackages
  :: Map Maintainer MaintainerPackage
  -> Map PackageName SCPackage
convertPackages =
  Map.fromList . map to . Map.toList
  where
    to :: (Maintainer, MaintainerPackage) -> (PackageName, SCPackage)
    to (maintainer, package) = (unSCPackageName $ mpName package, sc maintainer package)
    sc :: Maintainer -> MaintainerPackage -> SCPackage
    sc maintainer package = SCPackage
      { maintainers           = Set.singleton maintainer
      , versionRanges         = maybeToList . fmap unSCVersionRange $ mpRange package
      , flags                 = Map.mapKeys unSCFlagName . fromMaybe mempty $ mpFlags package
      , skippedBuild          = fromMaybe False $ mpSkippedBuild package
      , tests                 = fromMaybe ExpectSuccess $ mpTests package
      , benchmarks            = fromMaybe ExpectSuccess $ mpBenchmarks package
      , haddock               = fromMaybe ExpectSuccess $ mpHaddock package
      , tellMeWhenItsReleased = unSCVersion <$> mpTellMeWhenItsReleased package
      , hide                  = fromMaybe False $ mpHide package
      , noRevisions           = fromMaybe False $ mpNoRevisions package
      , nonParallelBuild      = fromMaybe False $ mpNonParallelBuild package
      , tarballUrl            = mpTarballUrl package
      }

loadStackageConstraints :: FilePath -> RIO env Constraints
loadStackageConstraints = decodeFileThrow >=> convert

loadStackageConstraintsBs :: ByteString -> RIO env Constraints
loadStackageConstraintsBs = decodeThrow >=> convert

convert :: SC -> RIO env Constraints
convert sc0 = do
  let (sc1, packages, errs) =
        foldl'
          go
          (sc0, mempty, [])
          (Map.toList $ scPackages sc0)
  unless (null errs) $ error $ unlines errs
  -- check that all of the fields are empty now
  pure Constraints
    { consGhcVersion = scGhcVersion sc1
    , consPackages = packages
    , consGithubUsers = scGithubUsers sc1
    }
  where
    go :: (SC, Map PackageName PackageConstraints, [String])
       -> (PackageName, SCPackage)
       -> (SC, Map PackageName PackageConstraints, [String])
    go (sc1, m, errs) (name, package) =
      case res of
        Left e -> (sc2, m, e : errs)
        Right pc -> (sc2, Map.insert name pc m, errs)
      where
        sc2 = sc1
        res = do
          source <- pure $
            case tarballUrl package of
              Nothing -> PSHackage $ HackageSource
                { hsRange =
                    case versionRanges package of
                      [] -> Nothing
                      r:rs -> Just $ foldl' intersectVersionRanges r rs
                , hsRequiredLatest = tellMeWhenItsReleased package
                , hsRevisions =
                    if noRevisions package
                      then NoRevisions
                      else UseRevisions
                }
              Just url -> PSUrl url

          Right PackageConstraints
            { pcMaintainers      = maintainers package
            , pcSource           = source
            , pcFlags            = flags package
            , pcSkipBuild        = skippedBuild package
            , pcNonParallelBuild = nonParallelBuild package
            , pcTests            = convertRunConfig $ tests package
            , pcBenchmarks       = convertRunConfig $ benchmarks package
            , pcHaddock          = convertRunConfig $ haddock package
            , pcHide             = hide package
            }
