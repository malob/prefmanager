{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Defaults where

import Defaults.Filter (Filter, filterDomainSet, filterDomainDiffs)
import Defaults.Pretty (prettyDomainDiffs)
import Defaults.Types (DomainDiff(..), Domains(..), Domain(..), DomainName(..), Key)

import Relude.Extra (un, wrap, traverseToSnd, keys)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Pool (withTaskGroup, async, wait)
import Control.Exception
  ( Exception(..), IOException, SomeAsyncException, SomeException
  , catch, displayException, fromException, handleJust, throwIO, tryJust )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as TIO
import Data.Text (stripEnd, splitOn)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Patience.Map (diff, isSame, toDelta)
import Prettyprinter (Doc, layoutPretty, defaultLayoutOptions, unAnnotate)
import qualified Prettyprinter.Render.Text as PRT
import Prettyprinter.Render.Terminal (AnsiStyle, putDoc)
import System.Console.ANSI (clearLine, setCursorColumn)
import System.Exit (ExitCode(..))
import Text.XML.HXT.Core (no, withSubstDTDEntities, withValidate)
import Text.XML.Plist (PlObject, fromPlDict, readPlistFromString)
import System.Process.Typed (proc, readProcess)

-- | Errors from running @/usr/bin/defaults@ or parsing its output. The
-- top-level command runner catches these and prints a clean message; the
-- watch loop catches them per domain and carries forward the previous
-- snapshot so a single failing domain doesn't tear down the watcher.
data DefaultsError
  = ProcessStartFailed [Text] Text
    -- ^ argv, message from the underlying 'IOException' (e.g.,
    -- @/usr/bin/defaults@ missing or otherwise unspawnable).
  | ProcessFailed [Text] ExitCode Text
    -- ^ argv, exit code, captured stderr.
  | DecodeFailed [Text] Text
    -- ^ argv, message describing the decode error.
  | PlistParseFailed DomainName Text
    -- ^ failing domain, error message from the plist parser.
  deriving Show

instance Exception DefaultsError where
  displayException = \case
    ProcessStartFailed args msg ->
      "failed to start `/usr/bin/defaults " <> toString (T.unwords args) <> "`: "
        <> toString msg
    ProcessFailed args ec err ->
      "`/usr/bin/defaults " <> toString (T.unwords args) <> "` " <> showExit ec
        <> if T.null err then "" else "\n" <> toString (T.stripEnd err)
    DecodeFailed args msg ->
      "`/usr/bin/defaults " <> toString (T.unwords args)
        <> "` produced non-UTF-8 output: " <> toString msg
    PlistParseFailed (DomainName d) msg ->
      "failed to parse plist for domain " <> toString d <> ": " <> toString msg
    where
      showExit (ExitFailure n) = "exited with status " <> show n
      showExit ExitSuccess     = "exited with success but signaled an error"

-- | Catch synchronous exceptions only. Async exceptions like 'UserInterrupt'
-- and 'ThreadKilled' propagate so Ctrl-C and supervisor shutdown work.
trySync :: IO a -> IO (Either SomeException a)
trySync = tryJust syncOnly

handleSync :: (SomeException -> IO a) -> IO a -> IO a
handleSync = handleJust syncOnly

syncOnly :: SomeException -> Maybe SomeException
syncOnly e = case fromException e :: Maybe SomeAsyncException of
  Just _  -> Nothing
  Nothing -> Just e

-- | Run @/usr/bin/defaults@ with the given argv. Captures stdout and stderr,
-- decodes stdout strictly as UTF-8, and throws 'DefaultsError' on spawn
-- failure, non-zero exit, or decode failure.
defaultsCmd :: [Text] -> IO Text
defaultsCmd args = do
  (ec, out, err) <- readProcess (proc "/usr/bin/defaults" (toString <$> args))
    `catch` \(e :: IOException) ->
      throwIO $ ProcessStartFailed args (toText (displayException e))
  case ec of
    ExitFailure _ -> throwIO $ ProcessFailed args ec (decodeStderr err)
    ExitSuccess   -> case TE.decodeUtf8' (LBS.toStrict out) of
      Left e  -> throwIO $ DecodeFailed args (toText (displayException e))
      Right t -> pure t
  where
    -- stderr is best-effort context; lenient decode preserves whatever
    -- bytes the process emitted even if they aren't valid UTF-8.
    decodeStderr = TE.decodeUtf8With lenientDecode . LBS.toStrict

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

-- | Runs @defaults export [domain] -@ and parses the output. Wraps any
-- synchronous exception from the plist parser as 'PlistParseFailed' so all
-- failures from this function surface as 'DefaultsError'.
export :: DomainName -> IO Domain
export d = do
  text <- defaultsCmd ["export", un d, "-"]
  pl   <- handleSync wrapParseError (parsePlist text)
  pure $ wrap $ fromList @(Map _ _) $ maybeToMonoid $ fromPlDict pl
  where
    wrapParseError e = throwIO $ PlistParseFailed d (toText (displayException e))

-- | Runs 'export' on the 'Set' of provided domains.
--
-- Uses a thread pool to run the exports in parallel with a limit of 100 threads to ensure we don't
-- run out of file descriptors. This limit is somewhat arbitrary.
exports :: Set DomainName -> IO Domains
exports ds = withTaskGroup 100 $ \tg -> do
  results <- forM (toList ds) $ \d -> async tg $ traverseToSnd export d
  wrap . fromList @(Map _ _) <$> mapM wait results

-- | Like 'exports', but tolerates per-domain 'DefaultsError's: returns
-- successful snapshots and a list of failures. Async exceptions and any
-- unexpected synchronous failures still propagate.
exportsTolerant :: Set DomainName -> IO (Map DomainName Domain, [(DomainName, DefaultsError)])
exportsTolerant ds = withTaskGroup 100 $ \tg -> do
  tasks <- forM (toList ds) $ \d -> async tg $ do
    r <- tryJust onlyDefaults (export d)
    pure $ case r of
      Left e    -> Left (d, e)
      Right dom -> Right (d, dom)
  partitionEithers <$> mapM wait tasks <&> \(errs, oks) ->
    (fromList oks, errs)
  where
    onlyDefaults :: SomeException -> Maybe DefaultsError
    onlyDefaults = fromException

diffDomain :: Domain -> Domain -> DomainDiff
diffDomain (Domain old) (Domain new) = wrap $ M.filter (not . isSame) $ diff old new

-- | Configuration for 'watch'.
data WatchOptions = WatchOptions
  { watchPlain    :: Bool    -- ^ Non-interactive output suitable for piping to a log.
  , watchInterval :: Int     -- ^ Microseconds to sleep between polls. 0 disables the sleep.
  , watchFilter   :: Filter  -- ^ Domain/key ignore filter.
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

    failedDomains = fromList . fmap fst

    logExportError (d, e) = TIO.hPutStrLn stderr $
      "warning: failed to export " <> un d <> ": " <> toText (displayException e)

    -- Sleep between polls (interruptible by async exceptions like Ctrl-C).
    -- A zero interval skips the call entirely, preserving the original
    -- as-fast-as-possible behavior for users who explicitly opt in.
    sleep = when (watchInterval > 0) $ threadDelay watchInterval

    goAnsi ds count old = do
      putText "Processing domains"
      when (count > 0) $ putText $ " (" <> show count <> " loops with no changes)"
      hFlush stdout
      (diffs, new) <- pollDiff ds old
      clearLine
      setCursorColumn 0
      unless (null diffs) $ putDoc $ prettyDomainDiffs diffs
      sleep
      goAnsi ds (if null diffs then count + 1 else 0) new

    goPlain ds old = do
      (diffs, new) <- pollDiff ds old
      unless (null diffs) $ do
        ts <- timestamp
        putTextLn $ "===== " <> ts <> " ====="
        putText $ renderPlain $ prettyDomainDiffs diffs
        hFlush stdout
      sleep
      goPlain ds new

    timestamp = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" <$> getCurrentTime

renderPlain :: Doc AnsiStyle -> Text
renderPlain = PRT.renderStrict . layoutPretty defaultLayoutOptions . unAnnotate

printKeys :: DomainName -> IO ()
printKeys = traverse_ putStrLn . keys @(Map _ PlObject) . un <=< export

printDomains :: IO ()
printDomains = traverse_ (putTextLn . un) =<< domains
