{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Defaults where

import Defaults.Pretty (prettyDomainDiffs)
import Defaults.Types (DomainDiff(..), Domains, Domain, DomainName(..))

import Control.Concurrent.Async (mapConcurrently)
import Data.Coerce (coerce)
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Patience.Map
import Prettyprinter.Render.Terminal (putDoc)
import System.IO (hFlush, stdout)
import System.Process (shell, readCreateProcess)
import Text.XML.HXT.Core (no, withSubstDTDEntities, withValidate)
import Text.XML.Plist (PlObject, fromPlDict, readPlistFromString)

-- | Convenience function for running macOS @defaults@ command.
defaultsCmd :: Text -> IO Text
defaultsCmd (T.unpack -> s) = T.pack <$> readCreateProcess (shell $ "defaults " <> s) ""

-- | Convenience function for parsing Plist strings
parsePlist :: Text -> IO PlObject
parsePlist = readPlistFromString [withValidate no, withSubstDTDEntities no] . T.unpack

-- | Gets list of domains by running @defaults domains@ and adds @NSGlobalDomain@ to the 'Set'.
domains :: IO (Set DomainName)
domains
  =   S.fromList
  .   coerce
  .   ("NSGlobalDomain" :)
  .   T.splitOn ", "
  .   T.stripEnd
  <$> defaultsCmd "domains"

-- | Runs @defaults export [domain] -@ and parses the output.
export :: DomainName -> IO Domain
export (coerce -> d)
  =   coerce
  .   M.fromList
  .   fromJust
  .   fromPlDict
  <$> (defaultsCmd ("export '" <> d <> "' -") >>= parsePlist)

-- | Runs 'export' on the 'Set' of provided domains
exports :: Set DomainName -> IO Domains
exports = (M.fromList <$>) . mapConcurrently (\d -> (d,) <$> export d) . S.toList

diffDomain :: Domain -> Domain -> DomainDiff
diffDomain old new = coerce $ M.filter (not . isSame) $ diff old new

-- | Watches a 'Set' of domains and prints any changes.
watch :: Set DomainName -> IO ()
watch ds = exports ds >>= (putStrLn "Watching..." >>) . go  where
  go :: Domains -> IO ()
  go old = do
    new <- exports ds
    let domainDiffs = uncurry diffDomain <$> toDelta (diff old new)
    (if null domainDiffs then pure () else putDoc $ prettyDomainDiffs domainDiffs)
      *> hFlush stdout
      *> go new
