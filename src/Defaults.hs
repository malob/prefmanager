{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Defaults where

import Defaults.Filter (Filter, filterDomainSet, filterDomainDiffs)
import Defaults.Pretty (prettyDomainDiffs)
import Defaults.Types (DomainDiff(..), Domains(..), Domain(..), DomainName(..), Key)

import Relude.Extra (un, wrap, traverseToSnd, keys)

import Control.Concurrent.Async.Pool (withTaskGroup, async, wait)
import Control.Exception (SomeException, displayException, try)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import Data.Text (stripEnd, splitOn)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Patience.Map (diff, isSame, toDelta)
import Prettyprinter (Doc, layoutPretty, defaultLayoutOptions, unAnnotate)
import qualified Prettyprinter.Render.Text as PRT
import Prettyprinter.Render.Terminal (AnsiStyle, putDoc)
import System.Console.ANSI (clearLine, setCursorColumn)
import Text.XML.HXT.Core (no, withSubstDTDEntities, withValidate)
import Text.XML.Plist (PlObject, fromPlDict, readPlistFromString)
import System.Process.Typed (proc, readProcessStdout_)

-- | Run @/usr/bin/defaults@ with the given argv. Bypasses the shell so that
-- domain names containing quotes or spaces are passed through verbatim.
defaultsCmd :: [Text] -> IO Text
defaultsCmd args =
  decodeUtf8 <$> readProcessStdout_ (proc "/usr/bin/defaults" (toString <$> args))

-- | Parse a plist XML 'Text'. DTD validation and entity substitution are
-- disabled because macOS's plist DTD reference is not always resolvable
-- offline.
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
  <$> defaultsCmd ["domains"]

-- | Runs @defaults export [domain] -@ and parses the output.
export :: DomainName -> IO Domain
export d
  =   wrap
  .   fromList @(Map _ _)
  .   maybeToMonoid
  .   fromPlDict
  <$> (defaultsCmd ["export", un d, "-"] >>= parsePlist)

-- | Runs 'export' on the 'Set' of provided domains.
--
-- Uses a thread pool to run the exports in parallel with a limit of 100 threads to ensure we don't
-- run out of file descriptors. This limit is somewhat arbitrary.
exports :: Set DomainName -> IO Domains
exports ds = withTaskGroup 100 $ \tg -> do
  results <- forM (toList ds) $ \d -> async tg $ traverseToSnd export d
  wrap . fromList @(Map _ _) <$> mapM wait results

-- | Per-domain export result for the watch loop. Failures are surfaced
-- rather than thrown so a single misbehaving domain (e.g., one that
-- disappears mid-poll) doesn't tear down the long-running watcher.
data ExportError = ExportError DomainName SomeException

-- | Like 'exports', but tolerates per-domain failures: returns successful
-- snapshots and a list of failures. Intended for the watch path; one-shot
-- commands should use 'exports' so genuine errors surface.
exportsTolerant :: Set DomainName -> IO (Map DomainName Domain, [ExportError])
exportsTolerant ds = withTaskGroup 100 $ \tg -> do
  tasks <- forM (toList ds) $ \d -> async tg $
    fmap (bimap (ExportError d) (d,)) (try @SomeException (export d))
  partitionEithers <$> mapM wait tasks <&> \(errs, oks) ->
    (fromList oks, errs)

diffDomain :: Domain -> Domain -> DomainDiff
diffDomain (Domain old) (Domain new) = wrap $ M.filter (not . isSame) $ diff old new

-- | Configuration for 'watch'.
data WatchOptions = WatchOptions
  { watchPlain  :: Bool    -- ^ Non-interactive output suitable for piping to a log.
  , watchFilter :: Filter  -- ^ Domain/key ignore filter.
  }

-- | Watches a 'Set' of domains and prints any changes.
watch :: WatchOptions -> Set DomainName -> IO ()
watch WatchOptions{..} ds0 = do
  let ds = filterDomainSet watchFilter ds0
  -- Baseline export tolerates per-domain failures the same way the polling
  -- loop does; otherwise a transient failure during startup (most likely
  -- with --all) would crash before any "warning + carry forward" handling
  -- could run.
  (oks, errs) <- exportsTolerant ds
  forM_ errs logExportError
  let initial = wrap oks :: Domains
  putTextLn "Watching..."
  when watchPlain $ hFlush stdout
  if watchPlain then goPlain ds initial else goAnsi ds 0 initial
  where
    -- Poll all domains, carrying forward the previous snapshot for any that
    -- failed to export so we don't emit spurious "domain removed" diffs.
    pollDiff ds old = do
      (oks, errs) <- exportsTolerant ds
      forM_ errs logExportError
      let oldMap = un old
          merged = M.union oks (M.restrictKeys oldMap (failedDomains errs))
          new    = wrap merged :: Domains
          allDiffs = uncurry diffDomain <$> toDelta (diff oldMap (un new))
          filtered = filterDomainDiffs watchFilter allDiffs
      pure (filtered, new)

    failedDomains = fromList . fmap (\(ExportError d _) -> d)

    logExportError (ExportError d e) = TIO.hPutStrLn stderr $
      "warning: failed to export " <> un d <> ": " <> toText (displayException e)

    goAnsi ds count old = do
      putText "Processing domains"
      when (count > 0) $ putText $ " (" <> show count <> " loops with no changes)"
      hFlush stdout
      (diffs, new) <- pollDiff ds old
      clearLine
      setCursorColumn 0
      if null diffs
        then goAnsi ds (count + 1) new
        else do
          putDoc $ prettyDomainDiffs diffs
          goAnsi ds 0 new

    goPlain ds old = do
      (diffs, new) <- pollDiff ds old
      unless (null diffs) $ do
        ts <- timestamp
        putTextLn $ "===== " <> ts <> " ====="
        putText $ renderPlain $ prettyDomainDiffs diffs
        hFlush stdout
      goPlain ds new

    timestamp = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" <$> getCurrentTime

renderPlain :: Doc AnsiStyle -> Text
renderPlain = PRT.renderStrict . layoutPretty defaultLayoutOptions . unAnnotate

printKeys :: DomainName -> IO ()
printKeys = traverse_ putStrLn . keys @(Map _ PlObject) . un <=< export

printDomains :: IO ()
printDomains = traverse_ (putTextLn . un) =<< domains
