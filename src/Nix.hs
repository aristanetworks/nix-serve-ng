module Nix (
  Stores,
  Store,
  priorityGroups,
  storeList,
  -- raw reexports from NixFFI
  PathInfo(..),
  getStoreDir,
  fingerprintPath,
  signString,
  -- wrapped reexports from NixFFI
  initStores,
  queryPathInfoFromHashPart,
  dumpPath,
  dumpLog,
  -- Logging to stdout
  LogLevel(..),
  Color(..),
  colored,
  logMsg,
  -- Convenience reexport from Aeson
  (.=)
) where

import Control.Applicative (empty)
import Control.Exception.Safe (Exception)
import Control.Monad (guard, join, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson ((.=))
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.IntMap (IntMap)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import System.IO (stdout)
import URI.ByteString (URIRef(..), Scheme(schemeBS), Query(queryPairs))
import UnliftIO.MVar (MVar, newMVar, readMVar, modifyMVar_)

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Chan qualified as Chan
import Control.Exception.Safe qualified as Exception
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as C8
import Data.Fixed qualified as Fixed
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Time.Clock qualified as Time
import Data.Time.LocalTime qualified as Time
import URI.ByteString qualified as URI

import NixFFI ( CppException(..), PathInfo(..)
              , getStoreDir, fingerprintPath, signString )
import Options (LogFormat(..))

import NixFFI qualified

type URI = URIRef URI.Absolute

-- `Store`s are keyed by priority
newtype Stores = Stores { stores :: IntMap [Store] }

data Store = Store
  { storeURL  :: ByteString
  , parsedURI :: URI
  , timeout   :: Word
  , logFormat :: LogFormat
  , storeVar  :: MVar StoreState
  }

priorityGroups :: Stores -> [[Store]]
priorityGroups = IntMap.elems . stores

storeList :: Stores -> [Store]
storeList = concat . priorityGroups

-- Tracking for timeout
data StoreState = StoreState
  { enabled   :: Enabled
  , failures  :: Int
  }

-- `Store` uniqueness by only scheme/path is enforced in `initStores`
instance Eq Store where
  l == r = (l.parsedURI.uriScheme, l.parsedURI.uriPath) == (r.parsedURI.uriScheme, r.parsedURI.uriPath)

instance Ord Store where
  compare l r = compare (l.parsedURI.uriScheme, l.parsedURI.uriPath) (r.parsedURI.uriScheme, r.parsedURI.uriPath)

data Enabled
  = PreInit
  | Enabled
  | Disabled { disabledUntil :: UTCTime }
  deriving stock Eq

data BadStoreURLs = BadStoreURLs [ByteString]
  deriving anyclass (Exception)
  deriving stock (Show)

data DuplicateStores = DuplicateStores [[ByteString]]
  deriving anyclass (Exception)
  deriving stock (Show)

-- Some store types may be slow to initialize, so we do it async
-- to allow queries against stores as soon as they come up.
initStores :: Maybe Word -> Maybe Int -> LogFormat -> [ByteString] -> IO Stores
initStores retryTimeout cores logFormat storeURLs = do
  let (bad, good) = partitionEithers $ map parseStoreURL storeURLs

  when (bad /= []) do Exception.throwIO $ BadStoreURLs bad

  let dups = filter ((> 1) . List.length) $ List.groupBy (\a b -> snd a == snd b) good
  when (dups /= []) do Exception.throwIO $ DuplicateStores (map (map fst) dups)

  timeout <- maybe NixFFI.getMaxConnectTimeout pure retryTimeout

  storesFlat <- mapM (uncurry $ preinitStore timeout logFormat) good

  let byPrio = IntMap.fromListWith (++) $ flip map storesFlat \store ->
        let prio = List.lookup "priority" store.parsedURI.uriQuery.queryPairs
                 >>= fmap fst . C8.readInt
        in  (fromMaybe 0 prio, [store])

  let width = List.maximum $ map List.length $ IntMap.elems byPrio
  maxCaps <- Concurrent.getNumCapabilities
  Concurrent.setNumCapabilities (max 1 . min maxCaps $ fromMaybe width cores)

  -- On the c++ side `openStore` is not thread safe and can be very slow
  -- for remote stores that are currently unreachable, so we want to
  -- initialize the local stores first.
  let localsFirst = uncurry (++) $ List.partition isLocalStoreType storesFlat
  retrying timeout initStore localsFirst

  pure $ Stores byPrio

retrying :: Word -> (a -> IO Bool) -> [a] -> IO ()
retrying timeout act xs = do
  chan <- Chan.newChan

  let retry x = do
        delaySecs timeout
        Chan.writeChan chan x

  let loop 0 = pure ()
      loop n = do
        x <- Chan.readChan chan
        success <- act x
        if success
        then loop (n-1)
        else retry x >> loop n

  Async.link =<< Async.async (loop $ List.length xs)
  for_ xs $ Chan.writeChan chan

initStore :: Store -> IO Bool
initStore store = do
  enabled <- NixFFI.initStore store.storeURL

  if enabled
  then resetStore   PreInit store
  else disableStore PreInit store

  pure enabled

queryPathInfoFromHashPart :: Store -> ByteString -> IO (Maybe (ByteString, PathInfo))
queryPathInfoFromHashPart store hashPart = do
  result <- withTimeout store \url -> runMaybeT do
    storePath <- MaybeT $ NixFFI.queryPathFromHashPart url hashPart
    pathInfo <- liftIO $ NixFFI.queryPathInfo url storePath
    pure (storePath, pathInfo)
  pure $ join result

dumpPath :: Store -> ByteString -> (Builder -> IO ()) -> IO ()
dumpPath store storePath builderCallback = do
  result <- withTimeout store \url -> NixFFI.dumpPath url storePath builderCallback
  maybe (Exception.throwIO $ StreamingException store storePath) pure result

data StreamingException = StreamingException Store ByteString
  deriving anyclass Exception

instance Show StreamingException where
  show (StreamingException store storePath) = C8.unpack $
    "Exception occurred while streaming " <> storePath <> " from " <> store.storeURL


dumpLog :: Store -> ByteString -> IO (Maybe ByteString)
dumpLog store baseName = do
  result <- withTimeout store \url -> NixFFI.dumpLog url baseName
  pure $ join result

parseStoreURL :: ByteString -> Either ByteString (ByteString, URI)
parseStoreURL url = either Left check parsed
  where
    parsed = parseURI url <> parseURI ("local:" <> url)
           & either (const $ Left url) Right
    parseURI = URI.parseURI URI.laxURIParserOptions

    check uri | isNormal || isSpecial || isPath = Right (url, uri)
              | otherwise = Left url
      where
        isNormal  = uri.uriScheme.schemeBS /= "local"
        isSpecial = uri.uriPath `elem` ["", "daemon", "local"]
        isPath    = C8.take 1 (C8.dropWhile (== '.') uri.uriPath) == "/"

isLocalStoreType :: Store -> Bool
isLocalStoreType store = store.parsedURI.uriScheme.schemeBS `elem` ["dummy","file","local","local-overlay","unix"]

preinitStore :: Word -> LogFormat -> ByteString -> URI -> IO Store
preinitStore timeout logFormat storeURL parsedURI = do
  storeVar <- newMVar StoreState { enabled = PreInit, failures = 0 }
  pure Store{storeURL, parsedURI, timeout, logFormat, storeVar}


-- Run a store operation if the store is enabled.
--
-- If the operation threw an exception on the C++ side,
-- the store disabled for `timeout` seconds and Nothing is returned.
--
-- Operations may be slow and we want to allow concurrent actions on a store,
-- so there is a possible race, but that's okay.
withTimeout :: Store -> (ByteString -> IO a) -> IO (Maybe a)
withTimeout store@Store{storeURL, storeVar} act = runMaybeT do
  StoreState{enabled} <- readMVar storeVar
  guard $ enabled == Enabled
  Exception.try (liftIO $ act storeURL) >>= \case
    Left CppException -> liftIO (disableStore Enabled store) >> empty
    Right value       -> liftIO (resetStore   Enabled store) >> pure value

-- Disable a store and fork a thread to reenable it after a delay
disableStore :: Enabled -> Store -> IO ()
disableStore restore store = modifyMVar_ store.storeVar \st@StoreState{enabled, failures} -> case enabled of
  Disabled{} -> pure st
  _ | otherwise -> do
        _ <- Async.link =<< Async.async do
          delaySecs store.timeout
          reenableStore restore store

        disabledSince <- Time.getCurrentTime
        let disabledUntil = Time.addUTCTime (fromIntegral store.timeout) disabledSince
        zonedUntil <- Time.utcToLocalZonedTime disabledUntil

        logMsg Info Red store "Disabled" ("until" .= zonedUntil <> "failures" .= failures)

        pure st{enabled = Disabled{disabledUntil}}

-- Allow operations to try using the store again. Increases the failure count.
reenableStore :: Enabled -> Store -> IO ()
reenableStore restore store = modifyMVar_ store.storeVar \st@StoreState{enabled, failures} -> case enabled of
  Disabled{} -> do
    when (restore == Enabled) do
      logMsg Info Green store "Enabled" ("failures" .= failures)
    pure st{enabled = restore, failures = failures + 1}
  _          -> pure st

-- After an operation succeeds, reset the failure count.
-- This also signals when a store has successfully initialized.
resetStore :: Enabled -> Store -> IO ()
resetStore prior store = modifyMVar_ store.storeVar \st@StoreState{enabled} -> case enabled of
  Disabled{} -> pure st
  _          -> do
    when (prior == PreInit) do
      logMsg Info Green store "Enabled" mempty
    pure st{enabled = Enabled, failures = 0}

delaySecs :: Real a => a -> IO ()
delaySecs = Concurrent.threadDelay . fromInteger
          . \case Fixed.MkFixed n -> n
          . fromRational @Fixed.Micro . toRational

data LogLevel =  Info | Debug

data Color = Red | Yellow | Green | Blue

logMsg :: LogLevel -> Color -> Store -> Builder -> Aeson.Series -> IO ()
logMsg level color store event series = when shouldLog do
  Builder.hPutBuilder stdout $ highlight event <> " " <> url <> " " <> json <> "\n"
  where
    shouldLog = case (level, store.logFormat) of
      (_    , Quiet         ) -> False
      (Info , _             ) -> True
      (Debug, Verbose       ) -> True
      (Debug, VerboseNoColor) -> True
      _                       -> False

    highlight = case store.logFormat of
      Verbose -> colored color
      _       -> id

    url | store.parsedURI.uriPath == "" = "daemon"
        | otherwise = Builder.byteString store.storeURL

    json = Aeson.fromEncoding $ Aeson.pairs series

colored :: Color -> Builder -> Builder
colored color str = "\o33[" <> ansiCode color <> "m" <> str <> "\o33[0m"

ansiCode :: Color -> Builder
ansiCode = \case
  Red    -> ";31"
  Yellow -> ";33"
  Green  -> ";32"
  Blue   -> ";34"
