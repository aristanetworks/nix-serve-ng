{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.CharSet.ByteSet (ByteSet(..))
import Data.Function ((&))
import Network.Socket (SockAddr(..))
import Network.Wai (Application)
import Numeric.Natural (Natural)
import Options (Options(..), Socket(..), SSL(..), LogFormat(..))

import qualified Control.Concurrent.Async             as Async
import qualified Control.Monad                        as Monad
import qualified Control.Monad.Except                 as Except
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString                      as ByteString
import qualified Data.ByteString.Char8                as ByteString.Char8
import qualified Data.ByteString.Builder              as Builder
import qualified Data.ByteString.Lazy                 as ByteString.Lazy
import qualified Data.CharSet.ByteSet                 as ByteSet
import qualified Data.List                            as List
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.Text.Encoding                   as Text
import qualified Data.Vector                          as Vector
import qualified Data.Void                            as Void
import qualified Network.HTTP.Types                   as Types
import qualified Network.Socket                       as Socket
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Handler.WarpTLS          as WarpTLS
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Options
import qualified Options.Applicative                  as Options
import qualified System.Environment                   as Environment

import Nix (Stores, PathInfo(..), LogLevel(..), Color(..))
import qualified Nix

data ApplicationOptions = ApplicationOptions
    { priority       :: Integer
    , storeDirectory :: ByteString
    , stores         :: Stores
    , secretKey      :: Maybe ByteString
    , logFormat      :: LogFormat
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
makeApplication opts request respond = do

    let stripStore = ByteString.stripPrefix (opts.storeDirectory <> "/")

    let done = Except.throwError

    let internalError :: Builder.Builder -> Except.ExceptT Wai.Response IO a
        internalError message = do
            let headers = [ ("Content-Type", "text/plain") ]

            let builder = "Internal server error: " <> message <> ".\n"

            let response =
                    Wai.responseBuilder
                        Types.status500
                        headers
                        builder

            done response

    let noSuchPath :: Except.ExceptT Wai.Response IO a
        noSuchPath = do
            let headers = [ ("Content-Type", "text/plain") ]

            let builder = "No such path.\n"

            let response =
                    Wai.responseBuilder
                        Types.status404
                        headers
                        builder

            done response

    let invalidPath :: Except.ExceptT Wai.Response IO a
        invalidPath = do
            let headers = [ ("Content-Type", "text/plain") ]

            let builder = "Invalid path.\n"

            let response =
                    Wai.responseBuilder
                        Types.status400
                        headers
                        builder

            done response

    let queryPathInfoFromHashPart hashPart = do
          let raceQuery = mapRace (flip Nix.queryPathInfoFromHashPart hashPart)
          result <- liftIO $ findJustM raceQuery (Nix.priorityGroups opts.stores)

          let logged (store, (storePath, pathInfo)) = do
                  let toValue = Aeson.toJSON . Text.decodeUtf8Lenient
                  liftIO $ Nix.logMsg Debug Blue store "Found" [ ("hashPart", toValue hashPart)
                                                               , ("storePath", toValue storePath) ]
                  pure (store, storePath, pathInfo)

          maybe noSuchPath logged result

    result <- Except.runExceptT do
        let rawPath = Wai.rawPathInfo request

        if  | Just prefix <- ByteString.stripSuffix ".narinfo" rawPath
            , Just hashPart <- ByteString.stripPrefix "/" prefix -> do
                Monad.unless (ByteString.length hashPart == 32 && validHashPart hashPart) do
                    invalidPath

                (_, storePath, pathInfo) <- queryPathInfoFromHashPart hashPart

                narHash2 <- case ByteString.stripPrefix "sha256:" pathInfo.narHash of
                    Nothing -> do
                        internalError "NAR hash missing sha256: prefix"
                    Just narHash2 -> do
                        return narHash2

                referenceNames <- case traverse stripStore pathInfo.references of
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
                    case pathInfo.deriver of
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

                signatures <- case opts.secretKey of
                    Just key -> do
                        signature <- liftIO (Nix.signString key fingerprint)

                        return (Vector.singleton signature)

                    Nothing -> do
                        return pathInfo.sigs

                let buildSignature signature =
                        "Sig: " <> Builder.byteString signature <> "\n"

                let builder =
                            "StorePath: "
                        <>  Builder.byteString storePath
                        <>  "\nURL: nar/"
                        <>  Builder.byteString hashPart
                        <>  "-"
                        <>  Builder.byteString narHash2
                        <>  ".nar\nCompression: none\nNarHash: "
                        <>  Builder.byteString pathInfo.narHash
                        <>  "\nNarSize: "
                        <>  Builder.word64Dec pathInfo.narSize
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
                        invalidPath

                Monad.unless (validHashPart hashPart) do
                    invalidPath

                (store, storePath, PathInfo{narHash}) <- queryPathInfoFromHashPart hashPart

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

                let streamingBody write flush = Nix.dumpPath store storePath callback
                      where callback builder = write builder >> flush

                let headers = [ ("Content-Type", "text/plain") ]

                let response =
                        Wai.responseStream Types.status200 headers streamingBody

                done response

            | Just suffix <- ByteString.stripPrefix "/log/" rawPath -> do
                let hashPart = ByteString.take 32 suffix

                Monad.unless (ByteString.length hashPart == 32 && validHashPart hashPart) do
                    invalidPath

                -- TODO: This could be split into a path query / dump like we do for nars
                -- so we can race the queries, but this isn't a hot path operation
                -- so there is less of a benefit.
                maybeBytes <- liftIO $ findJustM (flip Nix.dumpLog suffix) (Nix.storeList opts.stores)

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
                        <>  Builder.byteString opts.storeDirectory
                        <>  "\nWantMassQuery: 1\nPriority: "
                        <>  Builder.integerDec opts.priority
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

findJustM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findJustM act = go
  where
  go []     = pure Nothing
  go (x:xs) = act x >>= maybe (go xs) (pure . pure)

-- NOTE: This intentionally does not cancel the other asyncs
-- so it should only be used with short lived actions.
mapRace :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe (a, b))
mapRace _   []  = pure Nothing
mapRace act [x] = fmap (x,) <$> act x
mapRace act xs  = mapM (\x -> (fmap . fmap . fmap) (x,) . Async.async $ act x) xs >>= go
  where
  -- This use of `List.delete` is O(n^2) in the the length of xs,
  -- but for relatively small xs, it should still be cheap
  -- relative to the cost of `act`
  go [] = pure Nothing
  go as = do
    (a, v) <- Async.waitAny as
    maybe (go $ List.delete a as) (pure . pure) v

toSocket :: Natural -> FilePath -> IO Socket.Socket
toSocket backlog path = do
    let family = Socket.AF_UNIX

    Monad.unless (Socket.isSupportedFamily family) do
        fail "Unix domain sockets are not supported on this system"

    socket <- Socket.socket family Socket.Stream Socket.defaultProtocol

    Socket.bind socket (SockAddrUnix path)

    Socket.listen socket (fromIntegral backlog)

    return socket

readSecretKey :: FilePath -> IO ByteString
readSecretKey = fmap ByteString.Char8.strip . ByteString.readFile

main :: IO ()
main = do
    opts@Options{priority, logFormat} <- Options.execParser Options.parserInfo

    stores <- Nix.initStores opts.retryTimeout opts.cores logFormat $ NonEmpty.toList opts.storeURLs

    storeDirectory <- Nix.getStoreDir

    secretKeyFile <- Environment.lookupEnv "NIX_SECRET_KEY_FILE"

    secretKey <- traverse readSecretKey secretKeyFile

    logger <- case logFormat of
      Quiet                -> pure id
      Apache               -> pure RequestLogger.logStdout
      ApacheWithForwarding -> RequestLogger.mkRequestLogger RequestLogger.defaultRequestLoggerSettings
        { RequestLogger.outputFormat = RequestLogger.Apache RequestLogger.FromFallback }
      Verbose              -> pure RequestLogger.logStdoutDev
      VerboseNoColor       -> RequestLogger.mkRequestLogger RequestLogger.defaultRequestLoggerSettings
        { RequestLogger.outputFormat = RequestLogger.Detailed False }

    let application = logger (makeApplication ApplicationOptions{..})

    let sharedSettings =
            Warp.defaultSettings
                & Warp.setTimeout (fromIntegral opts.timeout)

    case opts of
        Options{ ssl = Disabled, socket = TCP{ host, port } } -> do
            let settings =
                    sharedSettings
                        & Warp.setHost host
                        & Warp.setPort port

            Warp.runSettings settings application

        Options{ ssl = Disabled, socket = Unix{ backlog, path } } -> do
            socket <- toSocket backlog path

            Warp.runSettingsSocket sharedSettings socket application

        Options{ ssl = Enabled{ cert, key }, socket = TCP{ host, port } } -> do
            let tlsSettings = WarpTLS.tlsSettings cert key

            let settings =
                    sharedSettings
                        & Warp.setHost host
                        & Warp.setPort port

            WarpTLS.runTLS tlsSettings settings application

        Options{ ssl = Enabled{ cert, key }, socket = Unix{ backlog, path } } -> do
            let tlsSettings = WarpTLS.tlsSettings cert key

            socket <- toSocket backlog path

            WarpTLS.runTLSSocket tlsSettings sharedSettings socket application
