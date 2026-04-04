{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module Curator.ProcessSpec (spec) where

import Control.Exception (try)
import Curator.Process (runWithEscalatingKill)
import GHC.Clock (getMonotonicTimeNSec)
import RIO (noLogging, runSimpleApp)
import RIO.Process (proc)
import System.Exit (ExitCode (..))
import System.Timeout (timeout)
import Test.Hspec

expectKilledWithin :: String -> Int -> Double -> IO () -> IO ()
expectKilledWithin label hardTimeoutUsec maxSecs action = do
  t0 <- getMonotonicTimeNSec
  result <- timeout hardTimeoutUsec (try @ExitCode action)
  t1 <- getMonotonicTimeNSec
  let elapsedSecs = fromIntegral (t1 - t0) / 1_000_000_000 :: Double
  case result of
    Nothing ->
      expectationFailure $
        label <> " timed out waiting for test completion; elapsed=" <> show elapsedSecs <> "s"
    Just (Left (ExitFailure 1)) ->
      elapsedSecs `shouldSatisfy` (< maxSecs)
    Just other ->
      expectationFailure $
        label <> " expected ExitFailure 1, got " <> show other <> "; elapsed=" <> show elapsedSecs <> "s"

spec :: Spec
spec =
  describe "runWithEscalatingKill" $ do

    it "completes normally when the process exits before the timeout"
      -- 'true' exits immediately with success; no signals should be sent.
      (runSimpleApp (noLogging (runWithEscalatingKill 1 1 (proc "true" []))) :: IO ())

    it "kills a SIGTERM-responsive process after the timeout fires" $ do
      -- If escalation is broken, this still ends naturally after a short time.
      expectKilledWithin "SIGTERM-responsive process" 3_500_000 2.8 $
        runSimpleApp $ noLogging $ runWithEscalatingKill 1 1 (proc "sleep" ["6"])

    it "kills a SIGTERM-resistant process with SIGKILL after the grace period" $ do
      -- Ignores SIGTERM but exits on its own after 6s if kill logic fails.
      expectKilledWithin "SIGTERM-resistant process" 3_500_000 2.8 $
        runSimpleApp $ noLogging $ runWithEscalatingKill 1 1
          (proc "sh" ["-c", "trap '' TERM; sleep 6; exit 0"])
