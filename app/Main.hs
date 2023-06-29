{-# LANGUAGE OverloadedStrings #-}
module Main where

import Defaults
import Defaults.Types (DomainName(..))

import Options.Applicative
import Relude.Extra (un)
import Data.Text (unpack)

-- | Main
main :: IO ()
main = join $ execParser opts

-- | App argument parser
opts :: ParserInfo (IO ())
opts = info
  (commands <**> helper)
  (fullDesc <> header "macOS Preferences Manager - a utility for working with macOS preferences.")

-- | App CLI commands
commands :: Parser (IO ())
commands = hsubparser
  (  command "watch"
       (info
         (   watch . fromList <$> some
               (DomainName <$> strArgument
                 (  metavar "DOMAIN..."
                 <> help "Domain(s) that will be watched."
                 <> completer domainCompleter
                 )
               )
         <|> flag' (watch =<< domains)
               (  long  "all"
               <> short 'a'
               <> help  "Watch all domains including NSGlobalDomain."
               )
         )
         $ progDesc "Watch domain(s) for changes."
       )
  <> command "domains"
       (info
         (pure printDomains)
         (progDesc "List all domains.")
       )
  <> command "keys"
       (info
         (printKeys . DomainName <$> strArgument
           (  metavar "DOMAIN"
           <> help "A domain for which to list keys."
           <> completer domainCompleter
           )
         )
         $ progDesc "List the current keys in a domain."
       )
  )
    where
      domainCompleter = listIOCompleter $ fmap (fmap unpack . un . toList) domains
