{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple tool to push all blobs from the Pantry database to Casa.

module Main where

import           Casa.Client
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Resource
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser
import qualified Data.Aeson.Types as Aeson
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Generics
import           Data.List hiding (deleteBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite hiding (LogFunc)
import           Database.Persist.TH
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Options.Applicative
import           Options.Applicative.Simple
import           Pantry hiding (runPantryAppWith)
import qualified Pantry as Pantry
import           Pantry.Internal.Stackage hiding (migrateAll)
import           RIO
import           RIO.Orphans
import           RIO.Process
import           System.Environment

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
LastPushed
  blobId BlobId
LastDownloaded
  hackageCabalId HackageCabalId
SnapshotLoaded
  name Text
  timestamp UTCTime
  Unique SnapshotLoadedNameUnique name
|]

data PantryStorage =
  PantryStorage
    { _pantryStoragePantry :: !PantryApp
    , _pantryStorageResourceMap :: !ResourceMap
    }

$(makeLenses ''PantryStorage)

instance HasLogFunc PantryStorage where
  logFuncL = pantryStoragePantry . logFuncL

instance HasResourceMap PantryStorage where
  resourceMapL = pantryStorageResourceMap

-- | To avoid confusion with the push url.
newtype PullUrl = PullUrl {pullUrlString :: String}
  deriving (Show)

-- | To avoid confusion with the pull url.
newtype PushUrl = PushUrl {pushUrlString :: String}
  deriving (Show)

data PushConfig =
  PushConfig
    { configSqliteFile :: Text
    , pushConfigConcurrentDownloads :: Int
    , pushConfigPushUrl :: PushUrl
    , pushConfigPullUrl :: PullUrl
    , pushConfigMaxBlobsPerRequest :: Int
    }
  deriving (Show)

-- | Command-line config.
pushConfigParser :: Parser PushConfig
pushConfigParser =
  PushConfig <$> sqliteFileParser <*>
  downloadConcurrencyParser <*>
  pushUrlParser <*>
  pullUrlParser <*>
  maxBlobsPerRequestParser

pushUrlParser :: Parser PushUrl
pushUrlParser =
  fmap PushUrl (strOption (long "push-url" <> metavar "URL" <> help "Casa push URL"))

pullUrlParser :: Parser PullUrl
pullUrlParser =
  fmap PullUrl (strOption (long "pull-url" <> metavar "URL" <> help "Casa pull URL"))

data PopulateConfig =
  PopulateConfig
    { populateConfigSnapshot :: Unresolved RawSnapshotLocation
    , populateConfigConcurrentDownloads :: Int
    , populateConfigPullUrl :: PullUrl
    }

-- | Command-line config.
populateConfigParser :: Parser PopulateConfig
populateConfigParser =
  PopulateConfig <$>
  fmap
    (parseRawSnapshotLocation . T.pack)
    (strOption
       (long "snapshot" <> metavar "SNAPSHOT" <>
        help "Snapshot in usual Stack format (lts-1.1, nightly-...)")) <*>
  downloadConcurrencyParser <*> pullUrlParser

data ContinuousConfig =
  ContinuousConfig
    { continuousConfigSleepFor :: Int
    , continuousConfigSqliteFile :: Text
    , continuousConfigConcurrentDownloads :: Int
    , continuousConfigPushUrl :: PushUrl
    , continuousConfigPullUrl :: PullUrl
    , continuousConfigMaxBlobsPerRequest :: Int
    , continuousConfigHackageLimit :: Maybe Int
    , continuousConfigSnapshotsLimit :: Maybe Int
    , continuousConfigResetPush :: Bool
    , continuousConfigResetPull :: Bool
    }

verboseParser :: Parser Bool
verboseParser = flag False True (long "verbose" <> short 'v' <> help "Verbose output")

resetPushParser :: Parser Bool
resetPushParser = flag False True (long "reset-push" <> help "Reset push cache")

resetPullParser :: Parser Bool
resetPullParser = flag False True (long "reset-pull" <> help "Reset pull cache")

continuousConfig :: Parser ContinuousConfig
continuousConfig =
  ContinuousConfig <$>
  option
    auto
    (long "sleep-for" <> help "Sleep for at least n minutes between polling" <>
     metavar "INT") <*>
  sqliteFileParser <*>
  downloadConcurrencyParser <*>
  pushUrlParser <*>
  pullUrlParser <*>
  maxBlobsPerRequestParser <*>
  optional
    (option
       auto
       (long "hackage-limit" <> help "Debug flag to pull n packages" <>
        metavar "INT")) <*>
  optional
    (option
       auto
       (long "snapshots-limit" <> help "Debug flag to pull n snapshots" <>
        metavar "INT")) <*>
  resetPushParser <*> resetPullParser

sqliteFileParser :: Parser Text
sqliteFileParser =
  fmap
    T.pack
    (strOption
       (long "sqlite-file" <> help "Filepath to use for sqlite database" <>
        metavar "PATH"))

downloadConcurrencyParser :: Parser Int
downloadConcurrencyParser =
  option
    auto
    (long "download-concurrency" <>
     help "How many package downloads to do at once" <>
     metavar "INT")

maxBlobsPerRequestParser :: Parser Int
maxBlobsPerRequestParser =
  option
    auto
    (long "max-blobs-per-request" <>
     help "How many package downloads to do at once" <>
     metavar "INT" <>
     value defaultCasaMaxPerRequest)

data App = App
  { logFunc :: LogFunc
  }

instance HasLogFunc App where
  logFuncL = lens logFunc (\x y -> x { logFunc = y })

-- | Main entry point.
main :: IO ()
main = do
  (verbose, runCmd) <-
    simpleOptions
      "0"
      "casa-curator"
      "casa-curator"
      verboseParser
      (do addCommand
            "push"
            "Push ALL blobs to Casa"
            pushCommand
            pushConfigParser
          addCommand
            "status"
            "Give some stats about the pantry database"
            (const statusCommand)
            (pure ())
          addCommand
            "populate"
            "Populate the pantry database with blobs from a given snapshot"
            populateCommand
            populateConfigParser
          addCommand
            "continuous-populate-push"
            "Poll stackage for new snapshots, \"populate\" then \"push\", repeatedly"
            continuousPopulatePushCommand
            continuousConfig)
  opts <- getLogOptions verbose
  withLogFunc opts (\logFunc -> runRIO (App logFunc) runCmd)

continuousPopulatePushCommand :: HasLogFunc env => ContinuousConfig -> RIO env ()
continuousPopulatePushCommand continuousConfig = do
  runSqlite
    (continuousConfigSqliteFile continuousConfig)
    (runMigration migrateAll)
  forever
    (do when
          (continuousConfigResetPull continuousConfig)
          (do logInfo "Resetting pull cache..."
              withContinuousProcessDb
                (continuousConfigSqliteFile continuousConfig)
                (do deleteWhere ([] :: [Filter LastDownloaded])))
        when
          (continuousConfigResetPush continuousConfig)
          (do logInfo "Resetting push cache..."
              withContinuousProcessDb
                (continuousConfigSqliteFile continuousConfig)
                (do deleteWhere ([] :: [Filter LastPushed])))
        pullAndPush
        delay)
  where
    delay = do
      logInfo
        ("Delaying for " <> display (continuousConfigSleepFor continuousConfig) <>
         " minutes.")
      threadDelay
        (1000 * 1000 * 60 * (continuousConfigSleepFor continuousConfig))
    pullAndPush = do
      logInfo "Getting hackage packages ..."
      newHackagePackages <- getNewHackagePackages continuousConfig
      logInfo "Getting new snapshots ..."
      newNames <- getNewSnapshots continuousConfig
      for_
        (maybe
           id
           take
           (continuousConfigSnapshotsLimit continuousConfig)
           (toList newNames))
        (\name -> do
           populateViaSnapshotTextName continuousConfig name
           withContinuousProcessDb
             (continuousConfigSqliteFile continuousConfig)
             (insertLoadedSnapshot name))
      logInfo "Initiating push ..."
      pushCommand
        PushConfig
          { pushConfigConcurrentDownloads =
              continuousConfigConcurrentDownloads continuousConfig
          , pushConfigPushUrl = continuousConfigPushUrl continuousConfig
          , pushConfigPullUrl = continuousConfigPullUrl continuousConfig
          , pushConfigMaxBlobsPerRequest =
              continuousConfigMaxBlobsPerRequest continuousConfig
          , configSqliteFile = continuousConfigSqliteFile continuousConfig
          }
      logInfo "Push done."

getNewSnapshots :: HasLogFunc env => ContinuousConfig -> RIO env (Set Text)
getNewSnapshots continuousConfig = do
  logSticky "Downloading snapshots from Stackage ..."
  availableNames <- downloadAllSnapshotTextNames
  logStickyDone "Downloaded snapshots from Stackage."
  loadedSnapshots <-
    withContinuousProcessDb
      (continuousConfigSqliteFile continuousConfig)
      (selectList [] [])
  let loadedNames =
        Set.fromList (map (snapshotLoadedName . entityVal) loadedSnapshots)
      newNames = Set.difference availableNames loadedNames
  logInfo ("There are " <> display (length newNames) <> " new snapshots.")
  pure newNames

getNewHackagePackages :: HasLogFunc env => ContinuousConfig -> RIO env ()
getNewHackagePackages continuousConfig = do
  mlastDownloadedHackageCabal :: Maybe HackageCabalId <-
    fmap
      (fmap (lastDownloadedHackageCabalId . entityVal))
      (liftIO
         (withContinuousProcessDb
            (continuousConfigSqliteFile continuousConfig)
            (selectFirst [] [])))
  runPantryAppWith
    (continuousConfigConcurrentDownloads continuousConfig)
    (pullUrlString (continuousConfigPullUrl continuousConfig))
    (continuousConfigMaxBlobsPerRequest continuousConfig)
    (do logInfo "Updating Hackage index ..."
        forceUpdateHackageIndex Nothing
        logInfo "Hackage index updated."
        count <-
          runPantryStorage (allHackageCabalCount mlastDownloadedHackageCabal)
        logInfo
          ("Will download " <> display count <> " Hackage cabal revisions.")
        logInfo
          ("Pulling from database ... " <>
           (case mlastDownloadedHackageCabal of
              Nothing -> ""
              Just sqlKey ->
                "(skipping past " <> display (fromSqlKey sqlKey) <> ")"))
        rplis <-
          runPantryStorage
            (allHackageCabalRawPackageLocations mlastDownloadedHackageCabal)
        logInfo "Done. Loading packages ..."
        hackageCabalIdChan <- newChan
        forM_
          (maybe
             id
             take
             (continuousConfigHackageLimit continuousConfig)
             (zip [1 :: Int ..] (M.toList rplis)))
          (downloadHackagePackage continuousConfig count)
        logStickyDone "Done downloading packages.")

downloadHackagePackage ::
  (Display a2, Display a1) =>
  ContinuousConfig
  -> a1
  -> (a2, (HackageCabalId, RawPackageLocationImmutable))
  -> RIO PantryApp ()
downloadHackagePackage continuousConfig count (i, (hackageCabalId, rpli)) = do
  logSticky ("[" <> display i <> "/" <> display count <> "] " <> display rpli)
  attempt
  where
    logit :: Exception e => e -> RIO PantryApp ()
    logit e =
      logStickyDone
        ("[" <> display i <> "/" <> display count <> "] " <>
         display (T.pack (displayException e)))
    attempt =
      catch
        (catch
           (void
              (do loadPackageRaw rpli
                  withContinuousProcessDb
                    (continuousConfigSqliteFile continuousConfig)
                    (do deleteWhere ([] :: [Filter LastDownloaded])
                        insert_
                          (LastDownloaded
                             {lastDownloadedHackageCabalId = hackageCabalId}))
                  logInfo ("Inserted package " <> display rpli)))
           (\e ->
              let
               in case e of
                    TreeWithoutCabalFile {} -> logit e
                    _ -> logit e))
        (\e@(HttpExceptionRequest _ statusCodeException) ->
           case statusCodeException of
             ResponseTimeout -> do
               logit e
               logSticky "Retrying ..."
               threadDelay (1000 * 1000)
               attempt
             StatusCodeException r _
               | getResponseStatusCode r == 403 -> logit e
             _ -> throwM e)

-- | Record that we've populated pantry with a snapshot.
insertLoadedSnapshot :: (MonadIO m) => Text -> ReaderT SqlBackend m ()
insertLoadedSnapshot name = do
  now <- liftIO getCurrentTime
  void
    (insertUnique
       (SnapshotLoaded
          {snapshotLoadedName = name, snapshotLoadedTimestamp = now}))

-- | Populate pantry via a text name of a snapshot.
populateViaSnapshotTextName :: HasLogFunc env => ContinuousConfig -> Text -> RIO env ()
populateViaSnapshotTextName continuousConfig snapshotTextName =
  runPantryAppWith
    (continuousConfigConcurrentDownloads continuousConfig)
    (pullUrlString (continuousConfigPullUrl continuousConfig))
    (continuousConfigMaxBlobsPerRequest continuousConfig)
    (do let unresoledRawSnapshotLocation =
              parseRawSnapshotLocation snapshotTextName
        rawSnapshot <-
          loadSnapshotByUnresolvedSnapshotLocation unresoledRawSnapshotLocation
        logInfo
          ("Populating from snapshot " <> display snapshotTextName <> " ...")
        populateFromRawSnapshot
          (continuousConfigConcurrentDownloads continuousConfig)
          rawSnapshot)

-- | With the database used for the continous process (to remember
-- what it has done).
withContinuousProcessDb ::
     MonadIO m => Text
  -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a
  -> m a
withContinuousProcessDb file =
  liftIO . runSqlite file

-- | Print a simple status of the database.
statusCommand :: HasLogFunc env => RIO env ()
statusCommand =
  runPantryAppWith
    0
    defaultCasaPullURL
    defaultCasaMaxPerRequest
    (runPantryStorage
       (do count <- allBlobsCount Nothing
           lift (logInfo ("Blobs in database: " <> display count))))

-- | Populate the pantry database.
populateCommand :: HasLogFunc env => PopulateConfig -> RIO env ()
populateCommand populateConfig =
  runPantryAppWith
    (populateConfigConcurrentDownloads populateConfig)
    (pullUrlString (populateConfigPullUrl populateConfig))
    defaultCasaMaxPerRequest
    (do rawSnapshot <-
          loadSnapshotByUnresolvedSnapshotLocation unresoledRawSnapshotLocation
        populateFromRawSnapshot
          (populateConfigConcurrentDownloads populateConfig)
          rawSnapshot)
  where
    unresoledRawSnapshotLocation = populateConfigSnapshot populateConfig

-- | Start pushing.
pushCommand :: HasLogFunc env => PushConfig -> RIO env ()
pushCommand config = do
  mlastBlobIdRef <- liftIO (newIORef Nothing)
  mlastPushedBlobId :: Maybe BlobId <-
    fmap
      (fmap (lastPushedBlobId . entityVal))
      (liftIO
         (withContinuousProcessDb (configSqliteFile config) (selectFirst [] [])))
  logInfo
    ("Pushing to " <> fromString (pushUrlString (pushConfigPushUrl config)))
  runPantryAppWith
    (pushConfigConcurrentDownloads config)
    (pullUrlString (pushConfigPullUrl config))
    (pushConfigMaxBlobsPerRequest config)
    (do blobs <-
          runPantryStorage
            (do count <- allBlobsCount mlastPushedBlobId
                if count > 0
                  then do
                    blobsSink
                      (pushUrlString (pushConfigPushUrl config))
                      (allBlobsSource mlastPushedBlobId .|
                       CL.mapM
                         (\(blobId, blob) -> do
                            liftIO (writeIORef mlastBlobIdRef (Just blobId))
                            pure blob) .|
                       stickyProgress count)
                  else pure ()
                pure count)
        when
          (blobs == 0)
          (logInfo "There are no new blobs to push since last time."))
  mlastBlobId <- liftIO (readIORef mlastBlobIdRef)
  logInfo
    ("Pushed to " <> fromString (pushUrlString (pushConfigPushUrl config)))
  logInfo
    ("Last blob pushed: " <>
     case mlastBlobId of
       Nothing -> "None recorded!"
       Just lastBlobId -> fromString (show lastBlobId))
  case mlastBlobId of
    Nothing -> pure ()
    Just lastBlobId ->
      liftIO
        (withContinuousProcessDb
           (configSqliteFile config)
           (do deleteWhere ([] :: [Filter LastPushed])
               insert_ (LastPushed {lastPushedBlobId = lastBlobId})))

-- | Output progress of blobs pushed.
stickyProgress ::
     (HasLogFunc env) => Int -> ConduitT i i (ReaderT r (RIO env)) ()
stickyProgress total = go (0 :: Int)
  where
    go i = do
      m <- await
      case m of
        Nothing ->
          lift (lift (logStickyDone ("Pushed " <> display total <> " blobs.")))
        Just v -> do
          let i' = i + 1
          when
            False
            (lift
               (lift
                  (logSticky
                     ("Pushing blobs: " <> display i' <> "/" <> display total))))
          yield v
          go i'

-- | Download all snapshots from stackage. The results are
-- paginated. We want everything, so we just keep increasing the page
-- index until we get a null result.
downloadAllSnapshotTextNames :: (HasLogFunc env) => RIO env (Set Text)
downloadAllSnapshotTextNames = go 1 mempty
  where
    go page acc = do
      request <-
        parseRequest ("https://www.stackage.org/snapshots?page=" ++ show page)
      response :: Response Aeson.Value <-
        let attempt =
              catch
                (httpJSON request)
                (\e@(HttpExceptionRequest _ statusCodeException) ->
                   case statusCodeException of
                     ResponseTimeout -> do
                       logStickyDone ("Timeout: " <> fromString (show e))
                       logSticky "Retrying ..."
                       threadDelay (1000 * 1000)
                       attempt
                     _ -> throwM e)
         in attempt
      case getResponseStatusCode response of
        200 ->
          case Aeson.parseEither snapshotsParser (getResponseBody response) of
            Left err -> error err
            Right snapshots -> loop (acc <> Set.fromList snapshots)
              where loop =
                      if null snapshots
                        then pure
                        else go (page + 1)
        _ -> error ("Failed to download: " ++ show (getResponseStatus response))

-- | Parse the JSON from Stackage.
snapshotsParser :: Aeson.Value -> Aeson.Parser [Text]
snapshotsParser j = do
  o <- Aeson.parseJSON j
  snapshots :: [[[Text]]] <- o Aeson..: "snapshots"
  concatenatedSnapshots <-
    fmap
      concat
      (mapM
         (\grouping ->
            mapM
              (\snapshotSpec ->
                 case listToMaybe snapshotSpec of
                   Nothing -> fail "No snapshot name"
                   Just name -> pure name)
              grouping)
         snapshots)
  pure concatenatedSnapshots

-- | Populate the database with packages from a raw snapshot.
populateFromRawSnapshot ::
     (HasLogFunc env, HasPantryConfig env, HasProcessContext env)
  => Int
  -> RawSnapshot
  -> RIO env ()
populateFromRawSnapshot concurrentDownloads rawSnapshot = do
  logInfo "Populating from snapshot..."
  let total = length (rsPackages rawSnapshot)
  pooledForConcurrentlyN_
    concurrentDownloads
    (zip [1 :: Int ..] (map rspLocation (M.elems (rsPackages rawSnapshot))))
    (\(i, rawPackageLocationImmutable) -> do
       logSticky
         ("Loading package: " <> display i <> "/" <> display total <> ": " <>
          display rawPackageLocationImmutable)
       loadPackageRaw rawPackageLocationImmutable)
  logStickyDone ("Loaded all " <> display total <> " packages.")

-- | Load a snapshot by its unresolved raw snapshot location (the
-- result of parsing from text).
loadSnapshotByUnresolvedSnapshotLocation ::
     (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Unresolved RawSnapshotLocation
  -> RIO env RawSnapshot
loadSnapshotByUnresolvedSnapshotLocation unresoledRawSnapshotLocation = do
  rawSnapshotLocation <- resolvePaths Nothing unresoledRawSnapshotLocation
  snapshotLocation <- completeSnapshotLocation rawSnapshotLocation
  loadSnapshot snapshotLocation

--------------------------------------------------------------------------------
-- Runners

-- | Get the log func used by everything in the app.
getLogOptions :: MonadIO f => Bool -> f LogOptions
getLogOptions verbose = fmap setOptions (logOptionsHandle stderr verbose)
  where
    setOptions =
      setLogUseTime verbose .
      setLogUseColor False .
      setLogUseLoc False .
      setLogTerminal False

-- | Our wrapper around Pantry's runPantryAppWith, to use our own
-- logger.
runPantryAppWith :: HasLogFunc env => Int -> String -> Int -> RIO PantryApp a -> RIO env a
runPantryAppWith maxConnCount casaPullURL casaMaxPerRequest f = do
  logFunc <- asks (view logFuncL)
  Pantry.runPantryAppWith
    maxConnCount
    casaPullURL
    casaMaxPerRequest
    (local (set logFuncL logFunc) f)

-- | Run database access inside Pantry's database.
runPantryStorage :: ReaderT SqlBackend (RIO PantryStorage) a -> RIO PantryApp a
runPantryStorage action = do
  pantryApp <- ask
  storage <- fmap (pcStorage . view pantryConfigL) ask
  withResourceMap
    (\resourceMap ->
       runRIO
         (PantryStorage
            {_pantryStorageResourceMap = resourceMap, _pantryStoragePantry = pantryApp})
         (withStorage_
            storage
            action))
