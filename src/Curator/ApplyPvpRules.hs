{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Curator.ApplyPvpRules
  ( applyPvpRules
  , mergeWithPreviousLtsBound
  ) where

import RIO
import Pantry
import Curator.Types
import Distribution.Types.VersionRange (VersionRange, earlierVersion, intersectVersionRanges, majorUpperBound)
import qualified RIO.Map as Map

mergeWithPreviousLtsBound :: Version -> Maybe VersionRange -> VersionRange
mergeWithPreviousLtsBound previousVersion mrange =
  let inferred = earlierVersion (majorUpperBound previousVersion)
   in case mrange of
        Nothing -> inferred
        Just range -> intersectVersionRanges inferred range

-- | Apply the PVP rules to a set of constraints by adding in additional upper bounds.
applyPvpRules
  :: Constraints
  -> Int -- ^ LTS major
  -> Int -- ^ LTS previous minor version
  -> RIO PantryApp Constraints
applyPvpRules constraints0 major minor = do
  (snapshot, _, _) <- loadAndCompleteSnapshotRaw (defaultSnapshotLocation $ LTS major minor) mempty mempty
  let getBounds name =
        case Map.lookup name (snapshotPackages snapshot) of
          Nothing -> Nothing
          Just sp -> Just (packageLocationVersion (spLocation sp))

      goSource name (PSHackage hs) =
        let range =
              case (getBounds name, hsRange hs) of
                (Nothing, Nothing) -> Nothing
                (Just x, Nothing) -> Just (mergeWithPreviousLtsBound x Nothing)
                (Nothing, Just y) -> Just y
                (Just x, Just y) -> Just (mergeWithPreviousLtsBound x (Just y))
         in PSHackage hs { hsRange = range }
      goSource _name (PSUrl url) = PSUrl url

      goPC name pc = pc { pcSource = goSource name (pcSource pc) }
      packages = Map.mapWithKey goPC (consPackages constraints0)
  pure constraints0 { consPackages = packages }
