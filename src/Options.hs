{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Options where

import Control.Applicative (optional, (<|>))
import Data.ByteString (ByteString)
import Data.String (IsString(..))
import Data.Void (Void)
import Network.Wai.Handler.Warp (HostPreference, Port)
import Numeric.Natural (Natural)
import Options.Applicative (Parser, ParserInfo, ReadM)
import Text.Megaparsec (Parsec)

import qualified Options.Applicative        as Options
import qualified Text.Megaparsec            as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Socket
    = TCP { host :: HostPreference, port :: Port }
    | Unix { backlog :: Natural, path :: FilePath }

data SSL = Disabled | Enabled { cert :: FilePath, key :: FilePath }

data Verbosity = Quiet | Normal | Verbose

data Options = Options
    { priority  :: Integer
    , socket    :: Socket
    , ssl       :: SSL
    , store     :: Maybe ByteString
    , timeout   :: Natural
    , verbosity :: Verbosity
    }

parseCert :: Parser FilePath
parseCert =
    Options.strOption
        (   Options.long "ssl-cert"
        <>  Options.help "The path to the SSL certificate file"
        <>  Options.metavar "FILE"
        )

parseKey :: Parser FilePath
parseKey =
    Options.strOption
        (   Options.long "ssl-key"
        <>  Options.help "The path to the SSL key file"
        <>  Options.metavar "FILE"
        )

parseSslEnabled :: Parser SSL
parseSslEnabled = do
    -- [NOTE: enable-ssl]
    --
    -- We parse this for backwards compatibility with nix-serve, but ignore
    -- it.  Instead, we use the presence of the --ssl-key and --ssl-cert
    -- options to detect whether SSL is enabled
    Options.switch
        (   Options.long "enable-ssl"
        <>  Options.internal
        )

    cert <- parseCert

    key <- parseKey

    return Enabled{..}

parseSsl :: Parser SSL
parseSsl = parseSslEnabled <|> pure Disabled

parseStore :: Parser (Maybe ByteString)
parseStore = optional $ Options.strOption
  (   Options.long "store"
  <>  Options.help "nix store uri. see: man nix3-help-stores"
  <>  Options.metavar "STORE"
  )

parseTcp :: Parser Socket
parseTcp = do
    host <- Options.strOption
        (   Options.long "host"
        <>  Options.help "The hostname to bind to"
        <>  Options.metavar "*|*4|!4|*6|!6|IPv4|IPv6"
            -- The default host for warp is "*4", but we specify a default of
            -- "*" for backwards compatibility with nix-serve, which defaults to
            -- binding to any IP address.  Also, binding to any IP address is
            -- probably a better default anyway than binding to only IPv4
            -- addresses.
        <>  Options.value "*"
        <>  Options.showDefaultWith (\_ -> "*")
        )

    port <- Options.option Options.auto
        (   Options.long "port"
        <>  Options.help "The port to bind to"
        <>  Options.metavar "0-65535"
            -- This is also for backwards compatibility with nix-serve, which
            -- defaults to port 5000.
        <>  Options.value 5000
        )

    return TCP{..}

defaultBacklog :: Natural
defaultBacklog = 1024

parseUnix :: Parser Socket
parseUnix = do
    backlog <- Options.option Options.auto
        (   Options.long "backlog"
        <>  Options.help "The maximum number of connections allowed for the backlog"
        <>  Options.metavar "INTEGER"
        <>  Options.value defaultBacklog
        )

    path <- Options.strOption
        (   Options.long "socket"
        <>  Options.short 'S'
        <>  Options.help "The socket to bind to"
        <>  Options.metavar "PATH"
        )

    return Unix{..}

parseSocket :: Parser Socket
parseSocket = parseTcp <|> parseUnix

parsePriority :: Parser Integer
parsePriority =
    Options.option Options.auto
        (   Options.long "priority"
        <>  Options.help "The priority of the cache (lower is higher priority)"
        <>  Options.metavar "INTEGER"
        <>  Options.value 30
        )

parseTimeout :: Parser Natural
parseTimeout =
    Options.option Options.auto
        (   Options.long "timeout"
        <>  Options.help "Timeout for requests"
        <>  Options.metavar "SECONDS"
            -- nix-serve does not timeout requests, but warp insists on a
            -- timeout, so we use the same default timeout as hydra
        <>  Options.value (10 * 60)
        )

parseVerbosity :: Parser Verbosity
parseVerbosity =
        Options.flag' Quiet
            (   Options.long "quiet"
            <>  Options.help "Disable logging"
            )
    <|> Options.flag' Verbose
            (   Options.long "verbose"
            <>  Options.help "Log verbosely"
            )
    <|> pure Normal

parseOptions :: Parser Options
parseOptions = do
    socket <- parseTcp <|> parseUnix

    ssl <- parseSsl

    priority <- parsePriority

    timeout <- parseTimeout

    store <- parseStore

    verbosity <- parseVerbosity

    return Options{..}

parseReader :: Parsec Void String a -> ReadM a
parseReader parser =
    Options.eitherReader \string -> do
        case Megaparsec.parse parser "(input)" string of
            Left bundle -> Left (Megaparsec.errorBundlePretty bundle)
            Right a     -> Right a

-- This is only for backwards compatibility with nix-serve, which supports
-- the --listen option
parseListen :: Parser Options
parseListen = do
    let parseTcpListen :: Parsec Void String Socket
        parseTcpListen = do
            host <- parseHost <|> pure "*"

            _ <- ":"

            port <- Lexer.decimal

            -- See: [NOTE: enable-ssl]
            _ <- optional ":ssl"

            return TCP{..}

    let parseUnixListen :: Parsec Void String Socket
        parseUnixListen = do
            let backlog = defaultBacklog

            path <- Megaparsec.takeRest

            return Unix{..}

    let parseSocketListen = parseTcpListen <|> parseUnixListen

    ssl <- parseSsl

    socket <- Options.option (parseReader parseSocketListen)
        (   Options.long "listen"
        <>  Options.short 'l'
        <>  Options.help "The TLS-enabled host and port to bind to"
        <>  Options.metavar "[HOST]:PORT|UNIX_SOCKET"
        <>  Options.hidden
        )

    priority <- parsePriority

    timeout <- parseTimeout

    store <- parseStore

    verbosity <- parseVerbosity

    return Options{..}

parserInfo :: ParserInfo Options
parserInfo =
    Options.info
        (Options.helper <*> (parseOptions <|> parseListen))
        (Options.progDesc "Serve the current Nix store as a binary cache")

parseHost :: Parsec Void String HostPreference
parseHost = do
    string <- Megaparsec.takeWhileP Nothing (/= ':')
    return (fromString string)
