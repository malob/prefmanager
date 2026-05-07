{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Defaults
import Defaults.Filter
  ( ConfigError, FilterOptions(..), IgnoreRule
  , buildFilter, builtinIgnores, configExamples
  , defaultIgnoreFilePath, formatRule, parseRule )
import Defaults.Types (DomainName(..))

import Options.Applicative
import Relude.Extra (un)
import Data.Text (unpack)

import Control.Exception (Exception, IOException, displayException, handle, throwIO)
import qualified Data.Text.IO as TIO
import GHC.IO.Exception (IOErrorType(ResourceVanished))
import System.IO (hIsTerminalDevice)
import System.IO.Error (ioeGetErrorType)

main :: IO ()
main = runCommand =<< execParser opts

-- | Run a chosen command with friendly error handling for our own typed
-- exceptions ('ConfigError', 'DefaultsError') and a clean exit on broken
-- pipes (e.g. when piping the watcher to @head@). Async exceptions
-- (Ctrl-C, 'ThreadKilled') and any unexpected synchronous exceptions
-- propagate so we don't disguise bugs as user errors.
runCommand :: IO () -> IO ()
runCommand =
    handle handleBrokenPipe
  . handle (reportError @ConfigError)
  . handle (reportError @DefaultsError)
  where
    reportError :: Exception e => e -> IO ()
    reportError e = do
      TIO.hPutStrLn stderr $ "Error: " <> toText (displayException e)
      exitFailure

    handleBrokenPipe :: IOException -> IO ()
    handleBrokenPipe e
      | ioeGetErrorType e == ResourceVanished = exitSuccess
      | otherwise = throwIO e

opts :: ParserInfo (IO ())
opts = info
  (commands <**> helper)
  (fullDesc <> header "macOS Preferences Manager - a utility for working with macOS preferences.")

domainCompleter :: Completer
domainCompleter = listIOCompleter $ fmap (fmap unpack . un . toList) domains

commands :: Parser (IO ())
commands = hsubparser
  (  command "watch"
       (info watchCmd $ progDesc "Watch domain(s) for changes")
  <> command "domains"
       (info (pure printDomains) (progDesc "List all domains"))
  <> command "keys"
       (info
         (printKeys . DomainName <$> strArgument
           (  metavar "DOMAIN"
           <> help "A domain for which to list keys"
           <> completer domainCompleter
           )
         )
         $ progDesc "List the current keys in a domain"
       )
  <> command "ignore-defaults"
       (info (pure printIgnoreDefaults)
         (progDesc "Print the built-in ignore patterns and the default config path"))
  )

watchCmd :: Parser (IO ())
watchCmd = run <$> plainParser <*> intervalParser <*> filterOptionsParser <*> targetParser
  where
    run plain interval fopts target = do
      flt <- buildFilter fopts
      ds  <- target
      -- Auto-switch to plain mode when stdout isn't a TTY (e.g. piped to
      -- a file or another process). Avoids leaking ANSI escapes into
      -- logs without making the user remember --plain.
      tty <- hIsTerminalDevice stdout
      watch WatchOptions
        { watchPlain    = plain || not tty
        , watchInterval = interval
        , watchFilter   = flt
        } ds

    plainParser = switch
      (  long "plain"
      <> help "Non-interactive output (no ANSI cursor tricks; timestamps each event). Use this when piping to a file."
      )

    intervalParser = option intervalReader
      (  long "interval"
      <> metavar "SECS"
      <> value (1 * 1_000_000)
      <> showDefaultWith (const "1")
      <> help "Polling interval in seconds (0 = no delay). Fractional values allowed."
      )

    intervalReader = eitherReader $ \s -> case readMaybe s :: Maybe Double of
      Nothing -> Left $ "expected a number, got: " <> s
      Just n
        | isNaN n || isInfinite n -> Left "interval must be a finite number"
        | n < 0                   -> Left "interval must be >= 0"
        | n * 1_000_000 > fromIntegral (maxBound :: Int) ->
            Left "interval too large"
        | otherwise               -> Right (round (n * 1_000_000))

    targetParser =
          (pure . WatchSpecific . fromList <$> some (DomainName <$> strArgument
            (  metavar "DOMAIN..."
            <> help "Domain(s) that will be watched"
            <> completer domainCompleter
            )))
      <|> flag' (pure WatchAll)
            (  long  "all"
            <> short 'a'
            <> help  "Watch all domains including NSGlobalDomain"
            )

filterOptionsParser :: Parser FilterOptions
filterOptionsParser = FilterOptions
  <$> fmap not (switch
        (  long "no-builtin-ignores"
        <> help "Disable the built-in ignore patterns"
        ))
  <*> many (option ignoreRuleReader
        (  long "ignore"
        <> metavar "DOMAIN[:KEY]"
        <> help "Ignore rule (repeatable). Both sides support * and ? globs. \
                \DOMAIN alone ignores the whole domain; DOMAIN:KEY ignores a specific key."
        ))
  <*> optional (strOption
        (  long "ignore-file"
        <> metavar "PATH"
        <> help "Path to a file of ignore rules (one per line, # for comments). \
                \Defaults to ~/.config/prefmanager/ignore.conf if it exists."
        ))

ignoreRuleReader :: ReadM IgnoreRule
ignoreRuleReader = eitherReader $ first toString . parseRule . toText

printIgnoreDefaults :: IO ()
printIgnoreDefaults = do
  path <- defaultIgnoreFilePath
  putTextLn $ "Default config file path: " <> toText path
  putTextLn ""
  putTextLn "Built-in ignore rules (disable with --no-builtin-ignores):"
  forM_ builtinIgnores $ \r -> putTextLn $ "  " <> formatRule r
  putTextLn ""
  putTextLn "Config file format:"
  putText configExamples
