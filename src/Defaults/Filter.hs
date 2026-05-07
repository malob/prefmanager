{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Defaults.Filter
  ( -- * Rules
    IgnoreRule
  , ignoreDomainPattern
  , ignoreKeyPattern
  , GlobPattern
  , globText
  , matchGlob
  , parseRule
  , formatRule
  , parseConfig
    -- * Filter
  , Filter
  , mkFilter
  , domainIsIgnored
  , keyIsIgnored
  , filterDomainSet
  , filterDomainDiffs
    -- * Config loading
  , FilterOptions(..)
  , defaultFilterOptions
  , buildFilter
  , defaultIgnoreFilePath
  , ConfigError(..)
    -- * Built-ins
  , builtinIgnores
  , configExamples
  ) where

import Defaults.Types (DomainName(..), DomainDiff(..), Key)

import Relude.Extra (un)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Control.Exception (Exception(..), throwIO)
import System.Directory (doesFileExist, getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))

-- | A precompiled glob pattern. Supports @*@ (any sequence) and @?@
-- (exactly one char). The 'GlobPattern' constructor is intentionally not
-- exported; callers must go through 'parseRule' (or the internal
-- 'mkGlobPattern' for built-ins) so unsupported metacharacters are caught
-- at construction time and the pattern is tokenized once instead of being
-- re-walked on every match.
data GlobPattern = GlobPattern
  { globText   :: Text          -- ^ Original pattern text, kept for display.
  , globTokens :: [GlobToken]   -- ^ Compiled token stream used by 'matchGlob'.
  } deriving (Eq, Ord, Show)

data GlobToken = GLit Char | GStar | GQuest deriving (Eq, Ord, Show)

mkGlobPattern :: Text -> Either Text GlobPattern
mkGlobPattern t
  | T.null t = Left "empty pattern"
  | Just c <- T.find unsupported t =
      Left $ "unsupported metacharacter " <> show c
          <> " in pattern (only * and ? are supported): " <> t
  | otherwise = Right (GlobPattern t (compile (toString t)))
  where
    unsupported c = c == '[' || c == ']' || c == '\\'
    compile = map $ \case
      '*' -> GStar
      '?' -> GQuest
      c   -> GLit c

-- | Match a precompiled glob pattern against a string.
matchGlob :: GlobPattern -> Text -> Bool
matchGlob g (toString -> s) = go (globTokens g) s
  where
    go []           []     = True
    go [GStar]      _      = True
    go (GStar:ps)   []     = go ps []
    go (GStar:ps)   (c:cs) = go ps (c:cs) || go (GStar:ps) cs
    go (GQuest:ps)  (_:cs) = go ps cs
    go (GLit p:ps)  (c:cs) = p == c && go ps cs
    go _            _      = False

-- | An ignore rule: a domain pattern, optionally narrowed to a key pattern.
-- The data constructor is hidden; build via 'parseRule'.
data IgnoreRule = IgnoreRule
  { ignoreDomainPattern :: GlobPattern
  , ignoreKeyPattern    :: Maybe GlobPattern
  } deriving (Eq, Ord, Show)

-- | Parse a single rule. Syntax:
--
-- > DOMAIN            -- ignore entire domain
-- > DOMAIN:KEY        -- ignore a key in a domain
--
-- Both sides support @*@ and @?@ globs. Splits on the first @:@ only, so keys
-- containing colons are preserved.
parseRule :: Text -> Either Text IgnoreRule
parseRule (T.strip -> raw)
  | T.null raw = Left "empty rule"
  | otherwise = case T.breakOn ":" raw of
      (d, "")   -> build d Nothing
      (d, rest) -> build d (Just (T.drop 1 rest))
  where
    build (T.strip -> d) mk
      | T.null d = Left $ "rule has empty domain: " <> raw
      | Just k <- mk, T.null (T.strip k) = Left $ "rule has empty key: " <> raw
      | otherwise = do
          dp <- annotate "domain" $ mkGlobPattern d
          kp <- traverse (annotate "key" . mkGlobPattern . T.strip) mk
          pure (IgnoreRule dp kp)
    annotate side = first (\m -> side <> " pattern: " <> m)

-- | Render a rule back to the syntax accepted by 'parseRule'. Inverse of
-- 'parseRule' (modulo whitespace).
formatRule :: IgnoreRule -> Text
formatRule (IgnoreRule d mk) =
  globText d <> maybe "" ((":" <>) . globText) mk

-- | Parse a rule that must be valid. Used for the built-in ignore list.
-- Errors at module load if a built-in pattern is malformed; this is a
-- development-time check, not a user-facing error path.
unsafeRule :: HasCallStack => Text -> IgnoreRule
unsafeRule t = case parseRule t of
  Right r -> r
  Left e  -> error $ "Defaults.Filter.unsafeRule: " <> e

-- | Parse a whole config file. Blank lines and lines starting with @#@
-- (after stripping whitespace) are skipped; every other line is parsed as
-- a single rule. Errors are reported with line numbers and accumulated so
-- the user sees every typo at once instead of fixing them one at a time.
--
-- Trailing @#@ comments are intentionally not supported: a literal @#@
-- inside a key would be ambiguous, and putting comments on their own lines
-- above each rule reads better anyway.
parseConfig :: Text -> Either [Text] [IgnoreRule]
parseConfig src = case partitionEithers (mapMaybe parseLine numbered) of
  ([], oks) -> Right oks
  (errs, _) -> Left errs
  where
    numbered = zip [1 :: Int ..] (T.strip <$> lines src)
    parseLine (n, t)
      | T.null t || "#" `T.isPrefixOf` t = Nothing
      | otherwise = Just $ first ((show n <> ": ") <>) (parseRule t)

-- | Compiled filter. Domain-only rules are kept separate so we can fast-path
-- skip whole domains during export.
data Filter = Filter
  { filterFullDomain :: [GlobPattern]
  , filterKeyRules   :: [(GlobPattern, GlobPattern)]
  } deriving Show

mkFilter :: [IgnoreRule] -> Filter
mkFilter rules = Filter
  { filterFullDomain = [ d | IgnoreRule d Nothing  <- rules ]
  , filterKeyRules   = [ (d, k) | IgnoreRule d (Just k) <- rules ]
  }

domainIsIgnored :: Filter -> DomainName -> Bool
domainIsIgnored Filter{..} d =
  any (`matchGlob` un d) filterFullDomain

keyIsIgnored :: Filter -> DomainName -> Key -> Bool
keyIsIgnored Filter{..} d k =
  any (\(dp, kp) -> matchGlob dp (un d) && matchGlob kp (toText k)) filterKeyRules

-- | Drop fully-ignored domains from a set (used to skip exporting them).
filterDomainSet :: Filter -> Set DomainName -> Set DomainName
filterDomainSet f = S.filter (not . domainIsIgnored f)

-- | Apply both whole-domain and per-key filtering to a set of diffs. Domains
-- whose diff becomes empty after filtering are dropped from the map.
filterDomainDiffs :: Filter -> Map DomainName DomainDiff -> Map DomainName DomainDiff
filterDomainDiffs f =
    M.mapMaybeWithKey trimDomain
  . M.filterWithKey (\d _ -> not (domainIsIgnored f d))
  where
    trimDomain d (DomainDiff diff) =
      let kept = M.filterWithKey (\k _ -> not (keyIsIgnored f d k)) diff
      in if null kept then Nothing else Just (DomainDiff kept)

-- | User-visible errors when loading or parsing the ignore config file.
-- These propagate as exceptions out of 'buildFilter' and are caught by the
-- top-level command runner, which prints a clean @Error: ...@ message.
data ConfigError
  = ConfigFileMissing FilePath
  | ConfigFileNotUtf8 FilePath
  | ConfigParseError FilePath [Text]
  deriving Show

instance Exception ConfigError where
  displayException = \case
    ConfigFileMissing p -> "ignore file not found: " <> p
    ConfigFileNotUtf8 p -> "ignore file is not valid UTF-8: " <> p
    ConfigParseError p errs -> toString $
      toText p <> ": parse errors:\n  " <> T.intercalate "\n  " errs

-- | CLI/file-driven options that combine into a 'Filter'. CLI rules are
-- already-parsed 'IgnoreRule's because @--ignore@ runs through
-- @optparse-applicative@'s 'eitherReader', so any syntax error fails at
-- option-parse time with the standard usage formatter.
data FilterOptions = FilterOptions
  { useBuiltinIgnores :: Bool
  , extraIgnoreRules  :: [IgnoreRule]
  , customIgnoreFile  :: Maybe FilePath
  } deriving Show

defaultFilterOptions :: FilterOptions
defaultFilterOptions = FilterOptions
  { useBuiltinIgnores = True
  , extraIgnoreRules  = []
  , customIgnoreFile  = Nothing
  }

-- | Default location of the ignore config: @$XDG_CONFIG_HOME/prefmanager/ignore.conf@.
defaultIgnoreFilePath :: IO FilePath
defaultIgnoreFilePath = do
  dir <- getXdgDirectory XdgConfig "prefmanager"
  pure (dir </> "ignore.conf")

-- | Build a 'Filter' from options, reading the config file if present.
-- Throws 'ConfigError' on file or parse errors; the top-level command
-- runner catches it and prints a clean message.
buildFilter :: FilterOptions -> IO Filter
buildFilter FilterOptions{..} = do
  fileRules <- loadFileRules customIgnoreFile
  let builtin = if useBuiltinIgnores then builtinIgnores else []
  pure $ mkFilter (builtin <> fileRules <> extraIgnoreRules)

loadFileRules :: Maybe FilePath -> IO [IgnoreRule]
loadFileRules custom = do
  path <- maybe defaultIgnoreFilePath pure custom
  exists <- doesFileExist path
  case (exists, custom) of
    -- The default file is optional; an explicit --ignore-file is treated as
    -- the user asserting it exists.
    (False, Nothing) -> pure []
    (False, Just _)  -> throwIO $ ConfigFileMissing path
    (True, _) -> do
      bytes <- readFileBS path
      contents <- case decodeUtf8' bytes of
        Right t -> pure t
        Left _  -> throwIO $ ConfigFileNotUtf8 path
      case parseConfig contents of
        Right rs   -> pure rs
        Left errs  -> throwIO $ ConfigParseError path errs

-- | Built-in ignores, on by default. These patterns cover macOS background
-- housekeeping that is universally noisy and not user-configurable. Disable
-- the whole list with @--no-builtin-ignores@ if you need to inspect raw state.
--
-- The strings are parsed via 'unsafeRule' so they share the user-facing
-- syntax exactly: a typo here crashes the program at startup with a useful
-- error rather than silently producing a malformed pattern.
--
-- /Provenance:/ this list is empirically derived from observing a running
-- macOS 26.x system. Apple changes internal daemon names and key shapes
-- between major releases, so expect drift over time — entries that no
-- longer fire are harmless, but new noise sources will need to be added.
builtinIgnores :: [IgnoreRule]
builtinIgnores = map unsafeRule
  -- AppKit window/toolbar UI state, written across many app domains as
  -- windows move and resize.
  [ "*:NSWindow Frame *"
  , "*:NSToolbar Configuration *"
  , "*:NSSplitView Subview Frames *"

  -- DuetKit "knowledge" framework throttling timestamps. macOS rewrites
  -- these continuously across com.apple.knowledge-agent, ContextStoreAgent,
  -- and similar DuetKit clients.
  , "*:_DKThrottledActivity*"

  -- Spotlight usage statistics.
  , "com.apple.Spotlight:engagementCount*"

  -- Mission Control / Spaces runtime state. Embeds window IDs, PIDs, and
  -- live layout data; the user-facing toggle ("Displays have separate
  -- Spaces") lives in a sibling key that is not filtered.
  , "com.apple.spaces:SpacesDisplayConfiguration"

  -- XPC scheduler bookkeeping for periodic activities.
  , "com.apple.xpc.activity2:ActivityBaseDates"

  -- Internal accessory-update flag.
  , "com.apple.PersonalAudio:shouldUpdateAccessory"

  -- CloudKit/iCloud housekeeping that fires on a timer regardless of user
  -- action. CloudKitAccountInfoCache appears across many daemons
  -- (syncdefaultsd, sociallayerd, passd, keyboardservicesd, ...), so we
  -- match the key in any domain.
  , "*:CloudKitAccountInfoCache"
  , "com.apple.routined:RTDefaultsSafetyCache*"
  , "com.apple.CloudKit:AccountInfoValidationCounter"
  , "*:OwnedDeviceLastPublishDate"
  , "com.apple.bird.containers.notifications:user_uid_*"
  , "com.apple.icloudmailagent:com.apple.icloud.mail.lastRetryTimestamp"

  -- Suggestions / TextUnderstanding bookmarks: fire across multiple
  -- *.TextUnderstandingObserver domains.
  , "*:deletionStreamBookmark"

  -- DuetKit app-prediction logging counters.
  , "com.apple.DuetExpertCenter.AppPredictionExpert:ATXUpdatePredictionsLogger*"

  -- Aerial wallpaper background fetch.
  , "com.apple.wallpaper.aerial:remoteResource*"

  -- APS push connection state and other periodic refresh timers.
  , "com.apple.jetpackassetd:apsLastKnownConnected"
  , "com.apple.NewDeviceOutreach:config-refresh-epoch"

  -- Persisted runtime UI state (derived from usage, not user-configured).
  , "com.apple.dock:trash-full"
  , "com.apple.finder:LastTrashState"
  , "com.apple.ActivityMonitor:OpenMainWindow"

  -- Auto-tracked "recent" lists. Users don't hand-edit these; they're
  -- updated whenever you navigate to a folder / launch an app.
  , "com.apple.finder:FXRecentFolders"
  , "com.apple.dock:recent-apps"

  -- Photos / mail / call-history / mmcs daemon timestamps and runtime flags.
  , "com.apple.Photos:IPXDefaultIsRestoringViewControllers"
  , "com.apple.photos.shareddefaults:PNUserDefaultPhotosAppLastLaunchDateKey"
  , "com.apple.mmcs:report.LastSuccessful*"
  , "*:CKStartupTime"
  , "com.apple.CallHistorySyncHelper:com.apple.callhistory.cloud-storage2"
  ]

-- | Example config-file content. Co-located with 'parseConfig' so the help
-- output we ship in @ignore-defaults@ is guaranteed to round-trip cleanly
-- through the parser.
configExamples :: Text
configExamples = T.unlines
  [ "# Lines starting with '#' are comments; blank lines are ignored."
  , ""
  , "# Ignore a whole domain"
  , "com.apple.spotlight"
  , ""
  , "# Ignore a specific key"
  , "com.apple.dock:recent-apps"
  , ""
  , "# Glob across all domains (* matches any sequence, ? matches one char)"
  , "*:NSWindow Frame *"
  ]
