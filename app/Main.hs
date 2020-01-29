module Main where

import           Control.Monad                  ( join )
import           Data.Coerce                    ( coerce )
import qualified Data.Set                      as S
import           Options.Applicative

import Defaults

-- | Main
main :: IO ()
main = join $ execParser opts

-- | App argument parser
opts :: ParserInfo (IO ())
opts = info
  (commands <**> helper)
  (fullDesc <> header "macOS Preferences Manager - a utility for working with macOS preferences")

-- | App CLI commands
commands :: Parser (IO ())
commands = hsubparser
  (command "watch"
    (info
      (   watch . S.fromList <$> some
            (Domain <$> strArgument
              (  metavar "DOMAIN..."
              <> completer (listIOCompleter $ fmap coerce . S.toList <$> domains)
              <> help "Domain(s) that will be watched"
              )
            )
      <|> flag' (watch =<< domains)
            (  long  "all"
            <> short 'a'
            <> help  "Watch all domains including NSGlobalDomain"
            )
      )
      (progDesc "Watch domain(s) for changes")
    )
  )
