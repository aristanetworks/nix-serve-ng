{-# LANGUAGE RecordWildCards #-}

module Options where

import Control.Applicative ((<|>), asum, optional)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Void (Void)
import Network.Wai.Handler.Warp (HostPreference, Port)
import Numeric.Natural (Natural)
import Options.Applicative (Parser, ParserInfo, ReadM)
import Text.Megaparsec (Parsec)

import qualified Data.List.NonEmpty              as NonEmpty
import qualified Options.Applicative             as Options
import qualified Options.Applicative.Help.Pretty as Options
import qualified Text.Megaparsec                 as Megaparsec
import qualified Text.Megaparsec.Char.Lexer      as Lexer

data Options = Options
    { socket       :: Socket
    , ssl          :: SSL
    , priority     :: Integer
    , timeout      :: Natural
    , logFormat    :: LogFormat
    , storeURLs    :: NonEmpty ByteString
    , retryTimeout :: Maybe Word
    , cores        :: Maybe Int
    }

data Socket
    = TCP { host :: HostPreference, port :: Port }
    | Unix { backlog :: Natural, path :: FilePath }

data SSL = Disabled | Enabled { cert :: FilePath, key :: FilePath }

data LogFormat = Quiet
               | Apache
               | ApacheWithForwarding
               | Verbose
               | VerboseNoColor

parserInfo :: ParserInfo Options
parserInfo = Options.info
    (Options.helper <*> parseOptions)
    (Options.progDesc "Serve the current Nix store as a binary cache")

parseOptions :: Parser Options
parseOptions = do
    socket <- parseSocket <|> parseListen
    priority <- parsePriority
    ssl <- parseSsl <|> pure Disabled
    timeout <- parseTimeout
    logFormat <- parseLogFormat
    storeURLs <- parseStores
    retryTimeout <- parseRetryTimeout
    cores <- parseCores
    return Options{..}

parseSocket :: Parser Socket
parseSocket = tcp <|> unix
    where
    tcp = do
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

    unix = do
      backlog <- Options.option Options.auto
          (   Options.long "backlog"
          <>  Options.help "The maximum number of connections allowed for the backlog"
          <>  Options.metavar "INTEGER"
          <>  Options.value 1024
          )

      path <- Options.strOption
          (   Options.long "socket"
          <>  Options.short 'S'
          <>  Options.help "The socket to bind to"
          <>  Options.metavar "PATH"
          )

      return Unix{..}

-- This is only for backwards compatibility with nix-serve, which supports
-- the --listen option
parseListen :: Parser Socket
parseListen = Options.option (parseReader $ tcp <|> unix)
    (   Options.long "listen"
    <>  Options.short 'l'
    <>  Options.help "The TLS-enabled host and port to bind to"
    <>  Options.metavar "[HOST]:PORT|UNIX_SOCKET"
    <>  Options.hidden
    )
    where
    tcp = do
        host <- parseHost <|> pure "*"
        _ <- ":"
        port <- Lexer.decimal
        _ <- optional ":ssl" -- See: [NOTE: enable-ssl]
        return TCP{..}

    unix = do
        path <- Megaparsec.takeRest
        return Unix{path, backlog = 1024}

    parseHost = do
        let ipv6 = Megaparsec.try do
              proto <- Megaparsec.takeWhileP Nothing (/= '[') <* "["
              addr <- Megaparsec.takeWhileP Nothing (/= ']') <* "]"
              _ <- Megaparsec.lookAhead ":"
              pure . fromString $ proto <> "[" <> addr <> "]"

        let other = Megaparsec.takeWhileP Nothing (/= ':')

        fmap fromString $ ipv6 <|> other


parsePriority :: Parser Integer
parsePriority = Options.option Options.auto
    (   Options.long "priority"
    <>  Options.help "The priority of the cache (lower is higher priority)"
    <>  Options.metavar "INTEGER"
    <>  Options.value 30
    )

parseSsl :: Parser SSL
parseSsl = do
    -- [NOTE: enable-ssl]
    --
    -- We parse this for backwards compatibility with nix-serve, but ignore
    -- it.  Instead, we use the presence of the --ssl-key and --ssl-cert
    -- options to detect whether SSL is enabled
    Options.switch
        (   Options.long "enable-ssl"
        <>  Options.internal
        )

    cert <- Options.strOption
        (   Options.long "ssl-cert"
        <>  Options.help "The path to the SSL certificate file"
        <>  Options.metavar "FILE"
        )

    key <- Options.strOption
        (   Options.long "ssl-key"
        <>  Options.help "The path to the SSL key file"
        <>  Options.metavar "FILE"
        )

    return Enabled{..}

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

parseLogFormat :: Parser LogFormat
parseLogFormat = asum [quiet, verbose, other]
  where
    quiet = Options.flag' Quiet
        (   Options.long "quiet"
        <>  Options.help "Disable logging. Alias for --log-format quiet"
        )
    verbose = Options.flag' Verbose
        (   Options.long "verbose"
        <>  Options.help "Verbose logging. Alias for --log-format verbose"
        )
    other = Options.option readLogFormat
        (   Options.long "log-format"
        <>  Options.metavar "FORMAT"
        <>  Options.helpDoc (Just helpDoc)
        <>  Options.value Apache
        )

    readLogFormat = Options.maybeReader \case
      "quiet" -> Just Quiet
      "default" -> Just Apache
      "real-ip" -> Just ApacheWithForwarding
      "verbose" -> Just Verbose
      "debug" -> Just VerboseNoColor
      _ -> Nothing

    helpDoc = Options.nest 2 $ Options.vsep
      [ "Use of of these output formats:"
      , "quiet   - do not log"
      , "normal  - Apache logging format (default)"
      , "real-ip - like normal, but resolves ip forwarding"
      , "verbose - verbose logging with colored output"
      , "debug   - verbose logging with uncolored output"
      ]


parseStores :: Parser (NonEmpty ByteString)
parseStores = fmap toNE . Options.many $ Options.strOption
  (   Options.long "store"
  <>  Options.helpDoc (Just helpDoc)
  <>  Options.metavar "STORE"
  )
  where
    toNE = fromMaybe (NonEmpty.singleton "") . NonEmpty.nonEmpty
    helpDoc = Options.vsep
        [ "nix store uri. see: man nix3-help-stores"
        , "Maybe be provided more than once. Respects the `priority` setting if given multiple stores."
        ]

parseRetryTimeout :: Parser (Maybe Word)
parseRetryTimeout = optional $ Options.option Options.auto
    (   Options.long "retry-timeout"
    <>  Options.helpDoc (Just helpDoc)
    <>  Options.metavar "SECONDS"
    )
    where
      helpDoc = Options.hsep
        [ "Timeout before retying a remote after a connection fails."
        , "Default to nix.settings.connect-timeout"
        ]

parseCores :: Parser (Maybe Int)
parseCores = optional $ Options.option Options.auto
    (   Options.long "cores"
    <>  Options.helpDoc (Just helpDoc)
    <>  Options.metavar "N"
    )
    where
      helpDoc = Options.hsep
        [ "The number of cores to use"
        , "Defaults to the largest number of --store args with the same priority."
        ]

parseReader :: Parsec Void String a -> ReadM a
parseReader parser =
    Options.eitherReader \string -> do
        case Megaparsec.parse parser "(input)" string of
            Left bundle -> Left (Megaparsec.errorBundlePretty bundle)
            Right a     -> Right a
