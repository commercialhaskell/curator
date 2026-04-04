{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Curator.Process
  ( runWithEscalatingKill
  ) where

import RIO
import RIO.Process
import System.Process (getPid)
import System.Posix.Signals (signalProcess, signalProcessGroup, sigTERM, sigKILL)
import qualified System.Timeout as ST

-- | Wrap a CPS-style process launcher (like @proc cmd args@) so that if the
-- process does not exit within @timeoutSecs@, SIGTERM is sent to its entire
-- process group; if it is still alive after @graceSecs@ more seconds, SIGKILL
-- is sent.  Running in a new process group ensures all descendants (e.g. a
-- stuck C test suite) are reachable.
runWithEscalatingKill
  :: (HasLogFunc env, HasProcessContext env)
  => Int                                              -- ^ outer timeout in seconds
  -> Int                                              -- ^ grace period after SIGTERM before SIGKILL
  -> ((ProcessConfig () () () -> RIO env ()) -> RIO env ())
  -> RIO env ()
runWithEscalatingKill timeoutSecs graceSecs withConfig =
  withConfig $ \config -> do
    let config' = setNewSession True config
    withProcess config' $ \p -> do
      let ph = unsafeProcessHandle p
      result <- liftIO $ ST.timeout (timeoutSecs * 1_000_000) (waitExitCode p)
      case result of
        Just _ -> checkExitCode p
        Nothing -> do
          logError $ "Build exceeded outer timeout of "
            <> display timeoutSecs
            <> " seconds; sending SIGTERM then SIGKILL to process group"
          liftIO $ do
            mPid <- getPid ph
            case mPid of
              Nothing -> pure ()
              Just pid -> do
                let processGroupId = fromIntegral pid
                void $ tryAny $ signalProcessGroup sigTERM processGroupId
                void $ tryAny $ signalProcess sigTERM processGroupId
                graceResult <- ST.timeout (graceSecs * 1_000_000) (waitExitCode p)
                case graceResult of
                  Just _ -> pure ()
                  Nothing -> do
                    void $ tryAny $ signalProcessGroup sigKILL processGroupId
                    void $ tryAny $ signalProcess sigKILL processGroupId
                    void $ waitExitCode p
          exitWith (ExitFailure 1)
