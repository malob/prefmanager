{-# LANGUAGE OverloadedStrings #-}
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

import Control.Exception (Exception(..), displayException, handle)
import qualified Data.Text.IO as TIO

main :: IO ()
main = runCommand =<< execParser opts

-- | Run a chosen command with friendly error handling for our own
-- 'ConfigError' exceptions; other exceptions propagate as bugs.
runCommand :: IO () -> IO ()
runCommand = handle reportConfigError
  where
    reportConfigError :: ConfigError -> IO ()
    reportConfigError e = do
      TIO.hPutStrLn stderr $ "Error: " <> toText (displayException e)
      exitFailure

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
watchCmd = run <$> watchOptionsParser <*> filterOptionsParser <*> targetParser
  where
    run plain fopts target = do
      flt <- buildFilter fopts
      ds  <- target
      watch WatchOptions { watchPlain = plain, watchFilter = flt } ds

    watchOptionsParser = switch
      (  long "plain"
      <> help "Non-interactive output (no ANSI cursor tricks; timestamps each event). Use this when piping to a file."
      )

    targetParser =
          (pure . fromList <$> some (DomainName <$> strArgument
            (  metavar "DOMAIN..."
            <> help "Domain(s) that will be watched"
            <> completer domainCompleter
            )))
      <|> flag' domains
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
