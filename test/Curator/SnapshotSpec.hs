{-# LANGUAGE OverloadedStrings #-}
module Curator.SnapshotSpec ( spec ) where

import Curator.Snapshot
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (unpack)
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.VersionRange (noVersion)
import Test.Hspec

spec :: Spec
spec = do
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
