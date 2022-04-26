{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Defaults where

import Defaults.Pretty (prettyDomainDiffs)
import Defaults.Types (DomainDiff(..), Domains(..), Domain(..), DomainName(..), Key)

import Relude.Extra (un, wrap, traverseToSnd, keys)

import Control.Concurrent.Async (mapConcurrently)
import Data.List (delete)
import qualified Data.Map.Strict as M
import Data.Text (stripEnd, splitOn)
import Patience.Map (diff, isSame, toDelta)
import Prettyprinter.Render.Terminal (putDoc)
import System.Console.ANSI (clearLine, setCursorColumn)
import Text.XML.HXT.Core (no, withSubstDTDEntities, withValidate)
import Text.XML.Plist (PlObject, fromPlDict, readPlistFromString)
import System.Process.Typed (shell, readProcessStdout_)

-- | Convenience function for running macOS @defaults@ command.
defaultsCmd :: Text -> IO Text
defaultsCmd (toString -> t) = decodeUtf8 <$> readProcessStdout_ (shell $ "/usr/bin/defaults " <> t)

-- | Convenience function for parsing Plist strings
parsePlist :: Text -> IO PlObject
parsePlist = readPlistFromString [withValidate no, withSubstDTDEntities no] . toString

-- | Gets list of domains by running @defaults domains@ and adds @NSGlobalDomain@ to the 'Set'.
domains :: IO (Set DomainName)
domains
  =   fromList
  .   wrap
  .   ("NSGlobalDomain" :)
  .   splitOn ", "
  .   stripEnd
  <$> defaultsCmd "domains"

-- | Runs @defaults export [domain] -@ and parses the output.
export :: DomainName -> IO Domain
export d
  =   wrap
  .   fromList @(Map _ _)
  .   maybeToMonoid
  .   fromPlDict
  <$> (defaultsCmd ("export '" <> un d <> "' -") >>= parsePlist)

-- | Runs 'export' on the 'Set' of provided domains
exports :: Set DomainName -> IO Domains
exports = wrap . fmap (fromList @(Map _ _)) . mapConcurrently (traverseToSnd export) . toList

diffDomain :: Domain -> Domain -> DomainDiff
diffDomain (Domain old) (Domain new) = wrap $ M.filter (not . isSame) $ diff old new

-- | Watches a 'Set' of domains and prints any changes.
watch :: Set DomainName -> IO ()
watch ds = exports ds >>= (putStrLn "Watching..." *>) . go 0 where
  go :: Int -> Domains -> IO ()
  go count old = do
    putText "Processing domains"
    if count > 0 then putText $ " (" <> show count <> " loops with no changes)"  else pass
    hFlush stdout
    new <- exports ds
    let domainDiffs = uncurry diffDomain <$> toDelta (diff (un old) (un new))
    clearLine
    setCursorColumn 0
    if null domainDiffs
      then go (count + 1) new
      else do
        putDoc $ prettyDomainDiffs domainDiffs
        go 0 new

-- | Print the keys of a given domain
printKeys :: DomainName -> IO ()
printKeys = traverse_ putStrLn . keys @(Map _ PlObject) . un <=< export

-- | Print the list of available domains
printDomains :: IO ()
printDomains = traverse_ (putTextLn . un) =<< domains
