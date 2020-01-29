{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Defaults where

import           Control.Concurrent.Async       ( mapConcurrently )
import           Data.Coerce                    ( coerce )
import           Data.List                      ( delete )
import           Data.Map                       ( Map(..) )
import qualified Data.Map                      as M
import           Data.Map.Delta
import           Data.Maybe
import           Data.Set                       ( Set(..) )
import qualified Data.Set                      as S
import           Text.Show.Pretty
import           Text.XML.Plist
import           Text.XML.HXT.Core
import           System.Process                 ( shell
                                                , readCreateProcess
                                                )

newtype Domain = Domain String deriving (Eq, Ord, Show)

-- | Convenience function for running macOS @defaults@ command.
defaultsCmd :: String -> IO String
defaultsCmd s = readCreateProcess (shell $ "defaults " <> s) ""

-- | Convenience function for parsing Plist strings
parsePlist :: String -> IO PlObject
parsePlist = readPlistFromString [withValidate no, withSubstDTDEntities no]

-- | Gets list of domains by running @defaults domains@ and adds @NSGlobalDomain@ to the 'Set'.
domains :: IO (Set Domain)
domains
  =   S.fromList
  .   (Domain "NSGlobalDomain" :)
  .   fmap (Domain . delete ',')
  .   words
  <$> defaultsCmd "domains"

-- | Runs @defaults export [domain] -@ and parses the output.
export :: Domain -> IO (Map String PlObject)
export (coerce -> d)
  =   M.fromList
  .   fromJust
  .   fromPlDict
  <$> (defaultsCmd ("export " <> d <> " -") >>= parsePlist)

-- | Runs 'export' on the 'Set' of provided domains
exports :: Set Domain -> IO (Map Domain (Map String PlObject))
exports = (M.fromList <$>) . mapConcurrently (\d -> (d,) <$> export d) . S.toList

-- | Watches a 'Set' of domains for changes.
watch :: Set Domain -> IO ()
watch ds = exports ds >>= go where
  go old = do
    new <- exports ds
    let plDiffs
          =   M.filter (not . null)
          .   toDelta
          .   (\(DeltaUnit o n) -> diff o n)
          <$> toDelta (diff old new)
    (if null plDiffs then pure () else pPrint plDiffs) *> go new
