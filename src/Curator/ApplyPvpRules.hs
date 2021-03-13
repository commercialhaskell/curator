{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Curator.ApplyPvpRules (applyPvpRules) where

import RIO
import Pantry
import Curator.Types
import Distribution.Types.VersionRange (intersectVersionRanges, majorBoundVersion)
import qualified RIO.Map as Map

-- | Apply the PVP rules to a set of constraints by adding in additional upper bounds.
applyPvpRules
  :: Constraints
  -> Int -- ^ LTS major
  -> Int -- ^ LTS previous minor version
  -> RIO PantryApp Constraints
applyPvpRules constraints0 major minor = do
  (snapshot, _, _) <- loadAndCompleteSnapshotRaw (defaultSnapshotLocation $ LTS major minor) mempty mempty
  let getBounds name =
        case Map.lookup name $ snapshotPackages snapshot of
          Nothing -> Nothing
          Just sp -> Just $ majorBoundVersion $ packageLocationVersion $ spLocation sp
  let goSource name (PSHackage hs) =
        let range =
              case (getBounds name, hsRange hs) of
                (Nothing, Nothing) -> Nothing
                (Just x, Nothing) -> Just x
                (Nothing, Just y) -> Just y
                (Just x, Just y) -> Just $ intersectVersionRanges x y
         in PSHackage hs { hsRange = range }
      goSource _name (PSUrl url) = PSUrl url
  let goPC name pc = pc { pcSource = goSource name $ pcSource pc }
  let packages = Map.mapWithKey goPC $ consPackages constraints0
  pure constraints0 { consPackages = packages }
