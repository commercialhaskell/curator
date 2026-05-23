{-# LANGUAGE OverloadedStrings #-}
module Curator.HackageDistro
    ( uploadHackageDistro
    ) where

import Curator.Types
import Data.ByteString.Builder (toLazyByteString)
import Distribution.Types.PackageName
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple (addRequestHeader)
import Network.HTTP.Types.Header (hAuthorization)
import Pantry
import RIO
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.Text as T

uploadHackageDistro ::
       (HasLogFunc env) => Target -> Map PackageName Version -> RIO env ()
uploadHackageDistro target packages = do
    man <- liftIO $ newManager tlsManagerSettings
    let tokenfile = "/hackage-distro-token"
    ecreds <- tryIO $ readFileBinary tokenfile
    case T.words $ decodeUtf8Lenient $ either (const mempty) id ecreds of
        [token] -> do
            logInfo $ "Uploading as Hackage distro: " <> display distroName
            res2 <- liftIO $
              uploadDistro distroName packages (encodeUtf8 token) man
            logInfo $ "Distro upload response: " <> displayShow res2
        _ -> error $ "No Hackage token found at " ++ tokenfile
  where
    distroName :: Text
    distroName =
        case target of
            TargetNightly _ -> "Stackage"
            TargetLts _ _ -> "LTSHaskell"

uploadDistro
    :: Text -- ^ distro name
    -> Map PackageName Version
    -> ByteString -- ^ Hackage distro token
    -> Manager
    -> IO (Response LByteString)
uploadDistro name packages distrotoken manager = do
    req1 <- parseRequest $ concat
        [ "https://hackage.haskell.org/distro/"
        , T.unpack name
        , "/packages.csv"
        ]
    let req2 = req1
                { requestHeaders = [("Content-Type", "text/csv")]
                , requestBody = RequestBodyLBS csv
                , method = "PUT"
                }
    httpLbs (addRequestHeader hAuthorization ("X-ApiKey " <> distrotoken) req2) manager
  where
    csv = toLazyByteString . getUtf8Builder
        $ mconcat
        $ L.intersperse "\n"
        $ map go
        $ Map.toList packages
    go (name', version) =
       displayShow (unPackageName name') <>
        "," <>
        displayShow (versionString version) <>
        "," <>
        displayShow ("https://www.stackage.org/package/" <> unPackageName name')
