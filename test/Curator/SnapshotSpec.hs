{-# LANGUAGE OverloadedStrings #-}
module Curator.SnapshotSpec ( spec ) where

import Curator (mergeWithPreviousLtsBound)
import Curator.Snapshot
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (unpack)
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.Version (mkVersion)
import Distribution.Types.VersionRange (VersionRange, noVersion, withinRange)
import Distribution.Text (simpleParse)
import Test.Hspec

spec :: Spec
spec = do
  describe "mergeWithPreviousLtsBound" $
    it "allows downgrades within an explicit user range in LTS minor mode" $ do
      let previous = mkVersion [0, 5, 5, 0]
      let userRange =
            simpleParse "^>= 0.5.4.0 && < 0.5.5.0"
              :: Maybe VersionRange

      case userRange of
        Nothing -> expectationFailure "Failed to parse user range"
        Just r -> do
          let merged = mergeWithPreviousLtsBound previous (Just r)
          withinRange (mkVersion [0, 5, 4, 1]) merged `shouldBe` True
          withinRange (mkVersion [0, 5, 5, 0]) merged `shouldBe` False

  describe "pkgBoundsError" $
    context "when there is more than one github ping" $
      it "returns github pings separated by spaces" $ do
        let pkg = mkPackageName "package1"
        let maintainers = S.fromList ["jsl", "fl", "fm"]
        let version = Nothing
        let isBoot = False
        let dependingPackage = DependingPackage{
          dpName = pkg,
          dpVersion = version,
          dpMaintainers = maintainers
        }
        let depBounds = DepBounds{
          dbRange = noVersion,
          dbComponents = S.empty
        }

        let users = M.fromList [(dependingPackage, depBounds)]

        unpack (pkgBoundsError pkg maintainers version isBoot users)
          `shouldContain` "fl, fm, jsl"
