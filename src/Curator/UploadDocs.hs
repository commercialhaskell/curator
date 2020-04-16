{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Curator.UploadDocs
    ( uploadDocs
    ) where

import Conduit
import Distribution.Package (PackageIdentifier(..))
import Pantry hiding (SHA256)
import RIO
import qualified RIO.Directory as D
import qualified RIO.FilePath as F
import RIO.List (stripPrefix)
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as BL
import RIO.Process
import RIO.FilePath ((</>))

import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Archive.Tar.Entry       as Tar

uploadDocs :: (HasLogFunc env, HasProcessContext env)
           => FilePath -- ^ directory containing docs
           -> Text -- ^ name of current docs, used as prefix in object names
           -> Text -- ^ bucket name
           -> RIO env ()
uploadDocs input' name bucket = do
    unlessM (D.doesDirectoryExist input') $ error $ "Could not find directory: " ++ show input'
    input <- fmap (</> "") $ D.canonicalizePath input'

    logInfo "Creating hoogle/orig.tar"
    hoogles <-
      runConduitRes $
      sourceDirectoryDeep False input .|
      foldMapC
        (\fp ->
          if isHoogleFile input fp
            then Set.singleton fp
            else Set.empty)
    hooglesLBS <- liftIO $ fmap Tar.write $ mapM toEntry $ Set.toList hoogles
    D.createDirectoryIfMissing True $ input </> "hoogle"
    BL.writeFile (input </> "hoogle" </> "orig.tar") hooglesLBS

    logInfo "Shelling out to AWS CLI to upload docs"
    proc
      "aws"
      [ "s3"
      , "cp"
      , "--recursive"
      , "--acl"
      , "public-read"
      , "--cache-control"
      , "maxage=31536000"
      , input
      , T.unpack $ "s3://" <> bucket <> "/" <> name <> "/"
      ]
      runProcess_

-- | Create a TAR entry for each Hoogle txt file. Unfortunately doesn't stream.
toEntry :: FilePath -> IO Tar.Entry
toEntry fp = do
    tp <- either error return $ Tar.toTarPath False $ F.takeFileName fp
    Tar.packFileEntry fp tp

isHoogleFile :: FilePath -> FilePath -> Bool
isHoogleFile input fp' = fromMaybe False $ do
    fp <- stripDirPrefix input fp'
    [pkgver, name] <- Just $ F.splitDirectories fp
    (pkg, ".txt") <- Just $ F.splitExtensions name
    PackageIdentifier pkg1 _ver <- parsePackageIdentifier pkgver
    pkg2 <- parsePackageName pkg
    return $ pkg1 == pkg2

stripDirPrefix :: FilePath -> FilePath -> Maybe FilePath
stripDirPrefix pref path = stripPrefix (F.addTrailingPathSeparator pref) path
