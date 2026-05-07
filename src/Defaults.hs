{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Defaults where

import Defaults.Filter (Filter, filterDomainSet, keyIsIgnored)
import Defaults.Pretty (prettyWatchEvents)
import Defaults.Types
  ( Delta(..), DomainDiff(..), Domains(..), Domain(..), DomainName(..)
  , Key, WatchEvent(..) )

import Relude.Extra (un, wrap, traverseToSnd, keys)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Pool (withTaskGroup, async, wait)
import Control.Exception
  ( Exception(..), IOException, SomeAsyncException, SomeException
  , catch, displayException, fromException, handleJust, throwIO, tryJust )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as Merge
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as TIO
import Data.Text (stripEnd, splitOn)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
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

-- | Compute the per-key diff between two domain snapshots. Equal values
-- are skipped at merge time so the result contains only @Old@, @New@,
-- and @Delta@ entries — the full set of changes, with no inert entries
-- to filter out afterwards.
diffDomain :: Domain -> Domain -> DomainDiff
diffDomain (Domain old) (Domain new) = DomainDiff $ Merge.merge
  (Merge.mapMissing (\_ x -> Old x))
  (Merge.mapMissing (\_ x -> New x))
  (Merge.zipWithMaybeMatched
     (\_ a b -> if a == b then Nothing else Just (Delta a b)))
  old new

-- | What 'watch' is targeting. Distinguishes a fixed user-named set from
-- the @--all@ case where the live domain list is re-checked each poll so
-- domains created post-startup become visible.
data WatchTarget
  = WatchSpecific (Set DomainName)
  | WatchAll
  deriving Show

-- | Configuration for 'watch'.
data WatchOptions = WatchOptions
  { watchPlain    :: Bool    -- ^ Non-interactive output suitable for piping to a log.
  , watchInterval :: Int     -- ^ Microseconds to sleep between polls. 0 disables the sleep.
  , watchFilter   :: Filter  -- ^ Domain/key ignore filter.
  }

-- | Per-domain state tracked across watch iterations. The five cases let
-- the loop distinguish: ordinary diffs (Snapshotted), a previously-good
-- domain that's currently failing (LostContact, carry forward for diff on
-- recovery), a baseline failure that never had content (NeverContacted,
-- silent recovery), a domain that appeared post-startup but failed its
-- first export (NewlyAppeared, render as "added" when it recovers), and
-- a domain that disappeared from the list after we knew it (Vanished,
-- removal already reported, retain snapshot for return-to-life diffs).
data DomainStatus
  = Snapshotted Domain
  | LostContact Domain
  | NeverContacted
  | NewlyAppeared
  | Vanished Domain
    -- ^ Was previously good (Snapshotted or LostContact) and then
    -- disappeared from @defaults domains@. We've already rendered the
    -- removal event and we keep the last snapshot so a return-to-life
    -- can diff against it instead of misfiring as a fresh add.
  deriving Show

-- | Watches a watch target and prints meaningful changes.
watch :: WatchOptions -> WatchTarget -> IO ()
watch WatchOptions{..} target = do
  initialWanted <- resolveTarget target
  let wanted0 = filterDomainSet watchFilter initialWanted
  when (null wanted0) emptySetGuard
  -- Baseline: populate the state map without rendering "Domain added"
  -- events. From the loop's perspective these were always there.
  (oks, errs) <- exportsTolerant wanted0
  forM_ errs warnFirstFailure
  let state0 = M.fromList $
        [(d, Snapshotted dom) | (d, dom) <- M.toList oks]
        <> [(d, NeverContacted) | (d, _) <- errs]
  putTextLn "Watching..."
  when watchPlain $ hFlush stdout
  if watchPlain
    then goPlain wanted0 state0 RefreshOk
    else goAnsi wanted0 0 state0 RefreshOk
  where
    emptySetGuard = do
      TIO.hPutStrLn stderr "Error: filter rules left no domains to watch."
      exitFailure

    -- Refresh the wanted set. Under @--all@ this re-runs `defaults
    -- domains`; on failure, warn once on the transition into a failing
    -- state and carry forward the previous set. Only catches
    -- 'DefaultsError' so unexpected synchronous bugs still fail loudly.
    refresh prev health = case target of
      WatchSpecific _ -> pure (prev, RefreshOk)
      WatchAll -> do
        r <- tryJust onlyDefaults (filterDomainSet watchFilter <$> domains)
        case r of
          Right ds -> pure (ds, RefreshOk)
          Left e -> do
            when (health == RefreshOk) $
              TIO.hPutStrLn stderr $
                "warning: failed to refresh domain list: "
                <> toText (displayException e)
            pure (prev, RefreshFailing)
      where
        onlyDefaults :: SomeException -> Maybe DefaultsError
        onlyDefaults = fromException

    -- One poll: refresh, export, classify, render, recurse.
    pollOnce !prevWanted !state !health = do
      (wanted, health') <- refresh prevWanted health
      (oks, errs) <- exportsTolerant wanted
      let errsMap = M.fromList errs
          -- Build per-domain results map covering every wanted domain.
          resultFor d = case M.lookup d oks of
            Just dom -> Right dom
            Nothing  -> Left (errsMap M.! d)
          step (events, st, ws) d =
            let (newSt, evt, warn) = classify (M.lookup d st) (resultFor d)
                events' = maybe events (\e -> M.insert d e events) evt
                ws'     = if warn then case M.lookup d errsMap of
                                         Just e  -> (d, e) : ws
                                         Nothing -> ws
                                  else ws
            in (events', M.insert d newSt st, ws')
          (eventsW, statePostWanted, warns) =
            foldl' step (M.empty, state, []) (toList wanted)
          -- Domains that vanished from the current `wanted` set: render
          -- a one-line "Domain removed" event for the first vanish
          -- (when prior status had a snapshot to count) and transition
          -- to 'Vanished'. Subsequent polls where the domain stays
          -- missing are no-ops. The retained snapshot lets a return-
          -- to-life diff against the prior state rather than misfire as
          -- a fresh "Domain added".
          vanishedNow = M.keysSet statePostWanted `S.difference` wanted
          (events, state') = foldl' vanishStep (eventsW, statePostWanted) (S.toList vanishedNow)
      forM_ (reverse warns) warnFirstFailure
      pure (filterEvents watchFilter events, wanted, state', health')

    vanishStep (events, st) d = case M.lookup d st of
      Just (Snapshotted dom@(Domain m)) ->
        ( M.insert d (EventRemoved (M.keys m) (M.size m)) events
        , M.insert d (Vanished dom) st )
      Just (LostContact dom@(Domain m)) ->
        ( M.insert d (EventRemoved (M.keys m) (M.size m)) events
        , M.insert d (Vanished dom) st )
      -- NeverContacted / NewlyAppeared: never had a snapshot to count,
      -- so no event. Leave them in state so a later reappearance still
      -- takes the appropriate baseline-recovery / NewlyAppeared path.
      -- Vanished: already reported on its first vanish, leave alone.
      _ -> (events, st)

    -- Sleep between polls (interruptible by async exceptions like Ctrl-C).
    -- A zero interval skips the call entirely, preserving the original
    -- as-fast-as-possible behavior for users who explicitly opt in.
    sleep = when (watchInterval > 0) $ threadDelay watchInterval

    goAnsi prevWanted count state health = do
      putText "Processing domains"
      when (count > 0) $ putText $ " (" <> show count <> " loops with no changes)"
      hFlush stdout
      (events, wanted, state', health') <- pollOnce prevWanted state health
      clearLine
      setCursorColumn 0
      unless (null events) $ putDoc $ prettyWatchEvents events
      sleep
      goAnsi wanted (if null events then count + 1 else 0) state' health'

    goPlain prevWanted state health = do
      (events, wanted, state', health') <- pollOnce prevWanted state health
      unless (null events) $ do
        ts <- timestamp
        putTextLn $ "===== " <> ts <> " ====="
        putText $ renderPlain $ prettyWatchEvents events
        hFlush stdout
      sleep
      goPlain wanted state' health'

    timestamp = toText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" <$> getZonedTime

-- | Track whether the @--all@ domain-list refresh is currently working.
-- Used to rate-limit refresh-failure warnings to one per failure run.
data RefreshHealth = RefreshOk | RefreshFailing
  deriving (Eq, Show)

warnFirstFailure :: (DomainName, DefaultsError) -> IO ()
warnFirstFailure (d, e) = TIO.hPutStrLn stderr $
  "warning: failed to export " <> un d <> ": " <> toText (displayException e)

-- | Resolve a 'WatchTarget' to the underlying set of domains. May run
-- @defaults domains@ for 'WatchAll'.
resolveTarget :: WatchTarget -> IO (Set DomainName)
resolveTarget (WatchSpecific ds) = pure ds
resolveTarget WatchAll           = domains

-- | Classify one domain's poll result against its prior status. Returns
-- the new status, an optional renderable event, and whether this poll
-- triggered a "first failure" warning.
classify
  :: Maybe DomainStatus
  -> Either DefaultsError Domain
  -> (DomainStatus, Maybe WatchEvent, Bool)
classify prior result = case (prior, result) of
  -- Newly appeared post-startup (untracked), exported successfully.
  (Nothing, Right dom@(Domain m)) ->
    (Snapshotted dom, Just (EventAdded (M.keys m) (M.size m)), False)
  -- Newly appeared post-startup, first export failed: warn once.
  (Nothing, Left _) ->
    (NewlyAppeared, Nothing, True)

  -- Stable snapshot, normal poll.
  (Just (Snapshotted old), Right new) ->
    case diffDomain old new of
      DomainDiff dm | M.null dm -> (Snapshotted new, Nothing, False)
                    | otherwise -> (Snapshotted new, Just (EventChanged (DomainDiff dm)), False)
  -- Was good, just lost contact: warn once, retain old snapshot.
  (Just (Snapshotted old), Left _) ->
    (LostContact old, Nothing, True)

  -- In-flight failure, no snapshot. Recovery is silent.
  (Just NeverContacted, Right dom) ->
    (Snapshotted dom, Nothing, False)
  (Just NeverContacted, Left _) ->
    (NeverContacted, Nothing, False)

  -- Appeared post-startup but its first export failed; the user hasn't
  -- seen it yet, so render as added on recovery.
  (Just NewlyAppeared, Right dom@(Domain m)) ->
    (Snapshotted dom, Just (EventAdded (M.keys m) (M.size m)), False)
  (Just NewlyAppeared, Left _) ->
    (NewlyAppeared, Nothing, False)

  -- Lost-contact recovery: diff against retained snapshot.
  (Just (LostContact old), Right new) ->
    case diffDomain old new of
      DomainDiff dm | M.null dm -> (Snapshotted new, Nothing, False)
                    | otherwise -> (Snapshotted new, Just (EventChanged (DomainDiff dm)), False)
  (Just (LostContact old), Left _) ->
    (LostContact old, Nothing, False)

  -- Vanished domain came back. If it's a recovery from a flake the
  -- diff is empty and we silently re-snapshot. If real changes
  -- happened during the absence, render them as an ordinary diff.
  (Just (Vanished old), Right new) ->
    case diffDomain old new of
      DomainDiff dm | M.null dm -> (Snapshotted new, Nothing, False)
                    | otherwise -> (Snapshotted new, Just (EventChanged (DomainDiff dm)), False)
  (Just (Vanished old), Left _) ->
    -- Reappeared in the domain list but its export is failing; treat
    -- as fresh lost contact and warn once.
    (LostContact old, Nothing, True)

-- | Apply per-key ignore rules to changed-domain events; added events
-- get the same per-key filter applied to the keys we list.
filterEvents :: Filter -> Map DomainName WatchEvent -> Map DomainName WatchEvent
filterEvents flt = M.mapMaybeWithKey trim
  where
    trim d (EventChanged (DomainDiff dm)) =
      let kept = M.filterWithKey (\k _ -> not (keyIsIgnored flt d k)) dm
      in if M.null kept then Nothing else Just (EventChanged (DomainDiff kept))
    trim d (EventAdded ks total) =
      let kept = filter (not . keyIsIgnored flt d) ks
      in if null kept then Nothing else Just (EventAdded kept total)
    trim d (EventRemoved ks total) =
      let kept = filter (not . keyIsIgnored flt d) ks
      in if null kept then Nothing else Just (EventRemoved kept total)

renderPlain :: Doc AnsiStyle -> Text
renderPlain = PRT.renderStrict . layoutPretty defaultLayoutOptions . unAnnotate

printKeys :: DomainName -> IO ()
printKeys = traverse_ putStrLn . keys @(Map _ PlObject) . un <=< export

printDomains :: IO ()
printDomains = traverse_ (putTextLn . un) =<< domains
