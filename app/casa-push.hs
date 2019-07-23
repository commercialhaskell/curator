{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple tool to push all blobs from the Pantry database to Casa.

module Main where

import Casa.Client
import Control.Lens.TH
import Control.Monad.Trans.Resource
import Data.Conduit
import Options.Applicative
import Pantry
import Pantry.Internal.Stackage
import RIO
import RIO.Orphans
import System.Environment

data CasaPush =
  CasaPush
    { _casaPushPantry :: !PantryApp
    , _casaPushResourceMap :: !ResourceMap
    }

$(makeLenses ''CasaPush)

instance HasLogFunc CasaPush where logFuncL = casaPushPantry . logFuncL
instance HasResourceMap CasaPush where resourceMapL = casaPushResourceMap

data Config =
  Config
    { configCasaUrl :: String
    }
  deriving (Show)

-- | Command-line config.
configParser :: Parser Config
configParser =
  Config <$>
  strOption (long "push-url" <> metavar "URL" <> help "Casa push URL")

-- | Main entry point.
main :: IO ()
main = do
  config <- execParser opts
  start config
  where
    opts =
      info
        (configParser <**> helper)
        (fullDesc <> progDesc "Run a program as a daemon with cron" <>
         header "cron-daemon - Run a program as a daemon with cron")

-- | Start pushing.
start :: MonadIO m => Config -> m ()
start config =
  runPantryApp
    (do pantryApp <- ask
        storage <- fmap (pcStorage . view pantryConfigL) ask
        withResourceMap
          (\resourceMap ->
             runRIO
               (CasaPush
                  { _casaPushResourceMap = resourceMap
                  , _casaPushPantry = pantryApp
                  })
               (withStorage_
                  storage
                  (do count <- allBlobsCount
                      blobsSink
                        (configCasaUrl config)
                        (allBlobsSource .| stickyProgress count)))))

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
          lift
            (lift
               (logSticky
                  ("Pushing blobs: " <> display i' <> "/" <> display total)))
          yield v
          go i'
