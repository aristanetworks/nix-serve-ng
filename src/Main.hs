{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.CharSet.ByteSet (ByteSet(..))
import Data.Function ((&))
import Network.Socket (SockAddr(..))
import Network.Wai (Application)
import Nix (PathInfo(..))
import Options (Options(..), Socket(..), SSL(..), Verbosity(..))
import Sysctl (_SO_MAX_CONN)

import qualified Control.Monad                        as Monad
import qualified Control.Monad.Except                 as Except
import qualified Data.ByteString                      as ByteString
import qualified Data.ByteString.Char8                as ByteString.Char8
import qualified Data.ByteString.Builder              as Builder
import qualified Data.ByteString.Lazy                 as ByteString.Lazy
import qualified Data.CharSet.ByteSet                 as ByteSet
import qualified Data.Vector                          as Vector
import qualified Data.Void                            as Void
import qualified Network.HTTP.Types                   as Types
import qualified Network.Socket                       as Socket
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Handler.WarpTLS          as WarpTLS
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Nix
import qualified Options
import qualified Options.Applicative                  as Options
import qualified System.BSD.Sysctl                    as Sysctl
import qualified System.Environment                   as Environment

data ApplicationOptions = ApplicationOptions
    { priority       :: Integer
    , storeDirectory :: ByteString
    , secretKey      :: Maybe ByteString
    }

-- https://github.com/NixOS/nix/blob/2.8.1/src/libutil/hash.cc#L83-L84
validHashPartBytes :: ByteSet
validHashPartBytes =
    ByteSet.fromList
        (   [ 0x30 .. 0x39 ]  -- 0..9
        <>  [ 0x61 .. 0x64 ]  -- abcd
        <>  [ 0x66 .. 0x6E ]  -- fghijklmn
        <>  [ 0x70 .. 0x73 ]  -- pqrs
        <>  [ 0x76 .. 0x7A ]  -- vwxyz
        )

validHashPart :: ByteString -> Bool
validHashPart hash = ByteString.all (`ByteSet.member` validHashPartBytes) hash

makeApplication :: ApplicationOptions -> Application
makeApplication ApplicationOptions{..} request respond = do
    let stripStore = ByteString.stripPrefix (storeDirectory <> "/")

    let done = Except.throwError

    let internalError message = do
            let headers = [ ("Content-Type", "text/plain") ]

            let builder = "Internal server error: " <> message <> ".\n"

            let response =
                    Wai.responseBuilder
                        Types.status500
                        headers
                        builder

            done response

    let noSuchPath = do
            let headers = [ ("Content-Type", "text/plain") ]

            let builder = "No such path.\n"

            let response =
                    Wai.responseBuilder
                        Types.status404
                        headers
                        builder

            done response

    result <- Except.runExceptT do
        let rawPath = Wai.rawPathInfo request

        if  | Just prefix <- ByteString.stripSuffix ".narinfo" rawPath
            , Just hashPart <- ByteString.stripPrefix "/" prefix -> do
                Monad.unless (ByteString.length hashPart == 32 && validHashPart hashPart) do
                    let headers = [ ("Content-Type", "text/plain") ]

                    let builder = "Invalid hash part.\n"

                    let response =
                            Wai.responseBuilder
                                Types.status400
                                headers
                                builder

                    done response

                maybeStorePath <- liftIO (Nix.queryPathFromHashPart hashPart)

                storePath <- case maybeStorePath of
                    Nothing        -> noSuchPath
                    Just storePath -> return storePath

                pathInfo@PathInfo{..} <- liftIO (Nix.queryPathInfo storePath)

                narHash2 <- case ByteString.stripPrefix "sha256:" narHash of
                    Nothing -> do
                        internalError "NAR hash missing sha256: prefix"
                    Just narHash2 -> do
                        return narHash2

                referenceNames <- case traverse stripStore references of
                    Nothing -> do
                        internalError "references missing store directory prefix"
                    Just names -> do
                        return names

                let referencesBuilder
                        | not (Vector.null referenceNames) =
                                "References:"
                            <>  foldMap (\name -> " " <> Builder.byteString name) referenceNames
                            <>  "\n"
                        | otherwise =
                            mempty

                deriverBuilder <-
                    case deriver of
                        Just d ->
                            case stripStore d of
                                Just name ->
                                    return
                                        (   "Deriver: "
                                        <>  Builder.byteString name
                                        <>  "\n"
                                        )

                                Nothing -> do
                                    internalError "deriver missing store directory prefix"

                        Nothing ->
                            return mempty

                fingerprint <- case Nix.fingerprintPath storePath pathInfo of
                    Nothing -> internalError "invalid NAR hash"
                    Just builder -> do
                        return (ByteString.Lazy.toStrict (Builder.toLazyByteString builder))

                signatures <- case secretKey of
                    Just key -> do
                        signature <- liftIO (Nix.signString key fingerprint)

                        return (Vector.singleton signature)

                    Nothing -> do
                        return sigs

                let buildSignature signature =
                        "Sig: " <> Builder.byteString signature <> "\n"

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
                        <>  foldMap buildSignature signatures

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

                done response

            | Just prefix <- ByteString.stripSuffix ".nar" rawPath
            , Just interior <- ByteString.stripPrefix "/nar/" prefix -> do
                let interiorLength = ByteString.length interior

                (hashPart, maybeExpectedNarHash) <- if
                    | interiorLength == 85
                    , (hashPart, rest) <- ByteString.splitAt 32 interior
                    , Just (0x2D, expectedNarHash) <- ByteString.uncons rest -> do
                        return (hashPart, Just (ByteString.Char8.pack "sha256:" <> expectedNarHash))

                    | interiorLength == 32 -> do
                        return (interior, Nothing)

                    | otherwise -> do
                        let headers = [ ("Content-Type", "text/plain") ]

                        let builder = "Invalid path component.\n"

                        let response =
                                Wai.responseBuilder
                                    Types.status400
                                    headers
                                    builder

                        done response

                Monad.unless (validHashPart hashPart) do
                    let headers = [ ("Content-Type", "text/plain") ]

                    let builder = "Invalid hash part.\n"

                    let response =
                            Wai.responseBuilder
                                Types.status400
                                headers
                                builder

                    done response

                maybeStorePath <- liftIO (Nix.queryPathFromHashPart hashPart)

                storePath <- case maybeStorePath of
                    Nothing        -> noSuchPath
                    Just storePath -> return storePath

                PathInfo{ narHash } <- liftIO (Nix.queryPathInfo storePath)

                Monad.unless (all (narHash ==) maybeExpectedNarHash) do
                    let headers = [ ("Content-Type", "text/plain") ]

                    let builder =
                            "Incorrect NAR hash. Maybe the path has been recreated.\n"

                    let response =
                            Wai.responseBuilder
                                Types.status404
                                headers
                                builder

                    done response

                maybeBytes <- liftIO (Nix.dumpPath hashPart)

                bytes <- case maybeBytes of
                    Nothing    -> noSuchPath
                    Just bytes -> return bytes

                let lazyBytes = ByteString.Lazy.fromStrict bytes

                let headers = [ ("Content-Type", "text/plain") ]

                let response =
                        Wai.responseLBS Types.status200 headers lazyBytes

                done response

            | Just suffix <- ByteString.stripPrefix "/log/" rawPath
            , 32 <= ByteString.length suffix -> do
                let hashPart = ByteString.take 32 suffix

                maybeBytes <- liftIO (Nix.dumpLog hashPart)

                bytes <- case maybeBytes of
                    Nothing    -> noSuchPath
                    Just bytes -> return bytes

                let lazyBytes = ByteString.Lazy.fromStrict bytes

                let headers = [ ("Content-Type", "text/plain") ]

                let response =
                        Wai.responseLBS Types.status200 headers lazyBytes

                done response

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

                done response

            | otherwise -> do
                let headers = [ ("Content-Type", "text/plain") ]

                let builder = "File not found.\n"

                let response =
                        Wai.responseBuilder Types.status404 headers builder

                done response

    case result of
        Left response -> respond response
        Right void    -> Void.absurd void

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

readSecretKey :: FilePath -> IO ByteString
readSecretKey = fmap ByteString.Char8.strip . ByteString.readFile

main :: IO ()
main = do
    options@Options{ priority, verbosity } <- do
        Options.execParser Options.parserInfo

    storeDirectory <- Nix.getStoreDir

    secretKeyFile <- Environment.lookupEnv "NIX_SECRET_KEY_FILE"

    secretKey <- traverse readSecretKey secretKeyFile

    let logger =
            case verbosity of
                Quiet   -> id
                Normal  -> RequestLogger.logStdout
                Verbose -> RequestLogger.logStdoutDev

    let application = logger (makeApplication ApplicationOptions{..})

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
