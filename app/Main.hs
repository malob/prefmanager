{-# LANGUAGE OverloadedStrings #-}
module Main where

import Defaults
import Defaults.Types (DomainName(..))

import Control.Monad (join)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import qualified Data.Set as S
import qualified Data.Text as T
import Options.Applicative

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
  (  command "watch"
       (info
         (   watch . S.fromList <$> some
               (DomainName <$> strArgument
                 (  metavar "DOMAIN..."
                 <> completer (listIOCompleter $ fmap (T.unpack . coerce) . S.toList <$> domains)
                 <> help "Domain(s) that will be watched"
                 )
               )
         <|> flag' (watch =<< domains)
               (  long  "all"
               <> short 'a'
               <> help  "Watch all domains including NSGlobalDomain"
               )
         )
         $ progDesc "Watch domain(s) for changes"
       )
  <> command "domains"
       (info
         (pure $ domains >>= traverse_ (putStrLn . T.unpack . coerce) . S.toList)
         (progDesc "List all domains")
       )
  )
