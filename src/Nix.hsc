{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TypeApplications         #-}

module Nix where

import Control.Monad.Managed (Managed)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Word (Word64)
import Foreign (Ptr, Storable(..))
import Foreign.C (CChar, CLong, CSize, CString)

import qualified Control.Exception     as Exception
import qualified Control.Monad.Managed as Managed
import qualified Data.ByteString       as ByteString
import qualified Data.Vector           as Vector
import qualified Data.Vector.Storable  as Vector.Storable
import qualified Foreign

#include "nix.hh"

foreign import ccall "freeString" freeString :: Ptr String_ -> IO ()

data String_ = String_ { data_ :: Ptr CChar, size  :: CSize }

instance Storable String_ where
    sizeOf _ = #{size struct string}

    alignment _ = #{alignment struct string}

    peek pointer = do
        data_ <- #{peek struct string, data} pointer

        size <- #{peek struct string, size} pointer

        return String_{ data_, size }

    poke pointer String_{ data_, size } = do
        #{poke struct string, data} pointer data_

        #{poke struct string, size} pointer size

fromString_ :: String_ -> IO ByteString
fromString_ String_{ data_, size } =
    ByteString.packCStringLen (data_, fromIntegral @CSize @Int size)

toString_ :: ByteString -> Managed String_
toString_ bytes = do
    (data_, size) <- Managed.managed (ByteString.useAsCStringLen bytes)

    return String_{ data_, size = fromIntegral @Int @CSize size }

foreign import ccall "freeStrings" freeStrings :: Ptr Strings -> IO ()

data Strings = Strings
    { data_ :: Ptr String_
    , size  :: CSize
    }

instance Storable Strings where
    sizeOf _ = #{size struct strings}

    alignment _ = #{alignment struct strings}

    peek pointer = do
        data_ <- #{peek struct string, data} pointer

        size <- #{peek struct string, size} pointer

        return Strings{ data_, size }

    poke pointer Strings{ data_, size } = do
        #{poke struct string, data} pointer data_

        #{poke struct string, size} pointer size

fromStrings :: Strings -> IO (Vector ByteString)
fromStrings Strings{ data_, size} = do
    foreignPointer <- Foreign.newForeignPtr_ data_

    let storableVector =
            Vector.Storable.unsafeFromForeignPtr0 foreignPointer
                (fromIntegral @CSize @Int size)

    traverse fromString_ (Vector.convert storableVector)

toStrings :: Vector ByteString -> Managed Strings
toStrings vector = do
    storableVector <- fmap Vector.convert (traverse toString_ vector)

    data_ <- Managed.managed (Vector.Storable.unsafeWith storableVector)

    let size = fromIntegral @Int @CSize (Vector.Storable.length storableVector)

    return Strings{ data_, size }

foreign import ccall "freePathInfo" freePathInfo :: Ptr CPathInfo -> IO ()

{-| We don't use the original @ValidPathInfo@ Nix type.  Rather, the C FFI
    defines a @pathinfo@ struct that wraps a subset of what we need in a
    C-compatible API
-}
data CPathInfo = CPathInfo
    { deriver    :: String_
    , narHash    :: String_
    , narSize    :: CLong
    , references :: Strings
    , sigs       :: Strings
    }

instance Storable CPathInfo where
    sizeOf _ = #{size struct PathInfo}

    alignment _ = #{alignment struct PathInfo}

    peek pointer = do
        deriver <- #{peek struct PathInfo, deriver} pointer

        narHash <- #{peek struct PathInfo, narHash} pointer

        narSize <- #{peek struct PathInfo, narSize} pointer

        references <- #{peek struct PathInfo, references} pointer

        sigs <- #{peek struct PathInfo, sigs} pointer

        return CPathInfo{ deriver, narHash, narSize, references, sigs }

    poke pointer CPathInfo{ deriver, narHash, narSize, references, sigs } = do
        #{poke struct PathInfo, deriver} pointer deriver

        #{poke struct PathInfo, narHash} pointer narHash

        #{poke struct PathInfo, narSize} pointer narSize

        #{poke struct PathInfo, references} pointer references

        #{poke struct PathInfo, sigs} pointer sigs

data PathInfo = PathInfo
    { deriver    :: Maybe ByteString
    , narHash    :: ByteString
    , narSize    :: Word64
    , references :: Vector ByteString
    , sigs       :: Vector ByteString
    } deriving (Show)

fromCPathInfo :: CPathInfo -> IO PathInfo
fromCPathInfo CPathInfo{ deriver, narHash, narSize, references, sigs } = do
    deriver_ <-
        if data_ (deriver :: String_) == Foreign.nullPtr
        then return Nothing
        else fmap Just (fromString_ deriver)

    narHash_ <- fromString_ narHash

    references_ <- fromStrings references

    sigs_ <- fromStrings sigs

    return PathInfo
        { deriver = deriver_
        , narHash = narHash_
        , narSize = fromIntegral @CLong @Word64 narSize
        , references = references_
        , sigs = sigs_
        }

foreign import ccall "getStoreDir" getStoreDir_ :: Ptr String_ -> IO ()

getStoreDir :: IO ByteString
getStoreDir =
    Foreign.alloca \output -> do
        let open = getStoreDir_ output
        let close = freeString output
        Exception.bracket_ open close do
            string_ <- peek output
            fromString_ string_

foreign import ccall "queryPathFromHashPart" queryPathFromHashPart_
    :: CString -> Ptr String_ -> IO ()

queryPathFromHashPart :: ByteString -> IO (Maybe ByteString)
queryPathFromHashPart hashPart = do
    ByteString.useAsCString hashPart \cHashPart -> do
        Foreign.alloca \output -> do
            let open = queryPathFromHashPart_ cHashPart output
            let close = freeString output
            Exception.bracket_ open close do
                string_@String_{ data_} <- peek output
                if data_ == Foreign.nullPtr
                    then return Nothing
                    else fmap Just (fromString_ string_)

foreign import ccall "queryPathInfo" queryPathInfo_
    :: CString -> Ptr CPathInfo -> IO ()

queryPathInfo :: ByteString -> IO PathInfo
queryPathInfo storePath = do
    ByteString.useAsCString storePath \cStorePath -> do
        Foreign.alloca \output -> do
            let open = queryPathInfo_ cStorePath output
            let close = freePathInfo output
            Exception.bracket_ open close do
                cPathInfo <- peek output
                fromCPathInfo cPathInfo


