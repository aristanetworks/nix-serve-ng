{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

{-| To benchmark `nix-serve-ng`, run the following commands:

    > $ nix build
    > $ PATH="./result/bin:${PATH}" cabal v1-bench --benchmark-option=--time-mode=wall

    You can compare against the old `nix-serve` by changing the first command
    to:

    > $ nix build --file '<nixpkgs>' nix-serve
-}
module Main where

import Control.Applicative (empty)
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import Numeric.Natural (Natural)
import Turtle (d)

import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Client as HTTP
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import qualified Test.Tasty.Bench as Bench
import qualified Turtle

prepare :: Natural -> IO (Vector FilePath)
prepare maxSize =
    Temp.withSystemTempFile "zeros" \tempFile handle -> do
        IO.hClose handle

        let generate i = do
                let size =
                        truncate
                            (   fromIntegral maxSize
                            ** (fromIntegral i / fromIntegral numFiles)
                            :: Double
                            )

                ByteString.writeFile tempFile (ByteString.replicate size 0)

                text <- Turtle.single do
                    Turtle.inproc "nix-store"
                        [ "--add", Text.pack tempFile ]
                        empty

                return (Text.unpack (Turtle.lineToText text))

        Vector.generateM (fromIntegral numFiles) generate

port :: Int
port = 8000

numFiles :: Natural
numFiles = 10

runNixServe :: IO ()
runNixServe =
   Turtle.procs "nix-serve"
       [ "--quiet", "--port", Turtle.format d port ]
       empty

host :: String
host = "http://localhost:" <> show port

main :: IO ()
main = do
    manager <- HTTP.newManager HTTP.defaultManagerSettings

    Async.withAsync runNixServe \_ -> do
        let fetchNarInfo file = do
                let hash = take 32 (drop 11 file)

                request <- HTTP.parseRequest (host <> "/" <> hash <> ".narinfo")

                response <- HTTP.httpLbs request manager

                return (HTTP.responseBody response)

        let fetchNar file = do
                bytes <- fetchNarInfo file

                let relativePath =
                        ( Char8.unpack
                        . ByteString.drop 5
                        . (!! 1)
                        . Char8.lines
                        ) bytes

                request <- HTTP.parseRequest (host <> "/" <> relativePath)

                response <- HTTP.httpLbs request manager

                return (HTTP.responseBody response)

        Bench.defaultMain
            [ Bench.env (prepare 1000000) \files -> do
                Bench.bench ("fetch present NAR info ×" <> show numFiles)
                    (Bench.nfAppIO (traverse_ fetchNarInfo) files)
            , Bench.bench "fetch absent NAR info ×1"
                (Bench.nfAppIO fetchNarInfo "/nix/store/00000000000000000000000000000000")
            , Bench.env (prepare 1000000) \files -> do
                Bench.bench ("fetch present NAR ×" <> show numFiles)
                    (Bench.nfAppIO (traverse_ fetchNar) files)
            ]
