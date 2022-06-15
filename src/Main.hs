{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Data.Function ((&))
import Network.Socket (SockAddr(..))
import Nix (PathInfo(..))
import Options (Options(..), Socket(..), SSL(..))
import Sysctl (_SO_MAX_CONN)

import qualified Control.Monad               as Monad
import qualified Data.ByteString             as ByteString
import qualified Data.ByteString.Lazy        as ByteString.Lazy
import qualified Data.ByteString.Builder     as Builder
import qualified Data.Vector                 as Vector
import qualified Network.HTTP.Types          as Types
import qualified Network.Socket              as Socket
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Nix
import qualified Options
import qualified Options.Applicative         as Options
import qualified System.BSD.Sysctl           as Sysctl

toSocket :: FilePath -> IO Socket.Socket
toSocket path = do
    let family = Socket.AF_UNIX

    Monad.unless (Socket.isSupportedFamily family) do
        fail "Unix domain sockets are not supported on this system"

    socket <- Socket.socket family Socket.Stream Socket.defaultProtocol

    Socket.bind socket (SockAddrUnix path)

    backlog <- Sysctl.sysctlReadInt _SO_MAX_CONN

    Socket.listen socket (fromIntegral backlog)

    return socket

main :: IO ()
main = do
    options@Options{ priority } <- Options.execParser Options.parserInfo

    storeDirectory <- Nix.getStoreDir

    let stripStore = ByteString.stripPrefix (storeDirectory <> "/")

    let application request respond = do
            let rawPath = Wai.rawPathInfo request

            if  | Just suffix <- ByteString.stripSuffix ".narinfo" rawPath
                , Just hashPart <- ByteString.stripPrefix "/" suffix -> do
                    maybeStorePath <- Nix.queryPathFromHashPart hashPart

                    case maybeStorePath of
                        Nothing -> do
                            let headers = [ ("Content-Type", "text/plain") ]

                            let builder = "No such path.\n"

                            let response =
                                    Wai.responseBuilder
                                        Types.status404
                                        headers
                                        builder

                            respond response
                        Just storePath -> do
                            PathInfo{..} <- Nix.queryPathInfo storePath

                            case ByteString.stripPrefix "sha256:" narHash of
                                Just narHash2 -> do
                                    let referencesBuilder
                                            | not (Vector.null references) =
                                                case traverse stripStore references of
                                                    Just names ->
                                                            "References:"
                                                        <>  foldMap (\name -> " " <> Builder.byteString name) names
                                                        <>  "\n"
                                            | otherwise =
                                                mempty

                                    let deriverBuilder =
                                            case deriver of
                                                Just d ->
                                                    case stripStore d of
                                                        Just name ->
                                                                "Deriver: "
                                                            <>  Builder.byteString name
                                                            <>  "\n"
                                                Nothing ->
                                                    mempty

                                    let builder =
                                                "StorePath: "
                                            <>  Builder.byteString storePath
                                            <>  "\nURL: nar/"
                                            <>  Builder.byteString hashPart
                                            <>  "-"
                                            <>  Builder.byteString narHash2
                                            <>  "\nCompression: none\nNarHash: "
                                            <>  Builder.byteString narHash
                                            <>  "\nNarSize: "
                                            <>  Builder.word64Dec narSize
                                            <>  "\n"
                                            <>  referencesBuilder
                                            <>  deriverBuilder

                                    let size =
                                            ( ByteString.Lazy.toStrict
                                            . Builder.toLazyByteString
                                            . Builder.int64Dec
                                            . ByteString.Lazy.length
                                            . Builder.toLazyByteString
                                            ) builder

                                    let headers =
                                            [ ("Content-Type", "text/x-nix-narinfo")
                                            , ("Content-Length", size)
                                            ]

                                    let response =
                                            Wai.responseBuilder
                                                Types.status200
                                                headers
                                                builder

                                    respond response

                                Nothing -> do
                                    let headers =
                                            [ ("Content-Type", "text/plain") ]

                                    let builder = "Internal Server Error"

                                    let response =
                                            Wai.responseBuilder
                                                Types.status500
                                                headers
                                                builder

                                    respond response

                | rawPath == "/nix-cache-info" -> do
                    let headers = [ ("Content-Type", "text/plain") ]

                    let builder =
                                "StoreDir: "
                            <>  Builder.byteString storeDirectory
                            <>  "\nWantMassQuery: 1\nPriority: "
                            <>  Builder.integerDec priority
                            <>  "\n"

                    let response =
                            Wai.responseBuilder Types.status200 headers builder

                    respond response

                | otherwise -> do
                    let headers = [ ("Content-Type", "text/plain") ]

                    let builder = "File not found.\n"

                    let response =
                            Wai.responseBuilder Types.status404 headers builder

                    respond response
 
    case options of
        Options{ ssl = Disabled, socket = TCP{ host, port } } -> do
            let settings =
                    Warp.defaultSettings
                        & Warp.setHost host
                        & Warp.setPort port

            Warp.runSettings settings application

        Options{ ssl = Disabled, socket = Unix{ path } } -> do
            let settings = Warp.defaultSettings

            socket <- toSocket path

            Warp.runSettingsSocket settings socket application

        Options{ ssl = Enabled{ cert, key }, socket = TCP{ host, port } } -> do
            let tlsSettings = WarpTLS.tlsSettings cert key

            let settings =
                    Warp.defaultSettings
                        & Warp.setHost host
                        & Warp.setPort port

            WarpTLS.runTLS tlsSettings settings application

        Options{ ssl = Enabled{ cert, key }, socket = Unix{ path } } -> do
            let tlsSettings = WarpTLS.tlsSettings cert key

            let settings = Warp.defaultSettings

            socket <- toSocket path

            WarpTLS.runTLSSocket tlsSettings settings socket application
