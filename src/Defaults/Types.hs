{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Defaults.Types where

import qualified Data.ByteString as BS
import qualified Data.Text as T

-- | Name of a domain, e.g., @NSGlobalDomain@, @com.apple.finder@, etc.
newtype DomainName = DomainName Text deriving (Eq, Ord, Show)

-- | A defaults key. 'Text' (not 'String') because we own the parser now;
-- the conversion at every glob-match and pretty-print site goes away.
type Key = Text

-- | Project-local representation of a plist value, replacing the
-- @PlObject@ from the upstream @plist@ library. Constructed by
-- 'Defaults.Plist.parsePlist'.
--
-- Notably:
--
--   * 'PvDict' is a list-of-pairs so we preserve the source order of
--     keys faithfully when rendering. Equality is overridden to be
--     order-insensitive (see the 'Eq' instance) so diffs treat dicts
--     as semantic dictionaries; duplicate keys are rejected at parse
--     time by the parser.
--   * 'PvArray' is a plain list; its order is semantic in plists.
--   * 'PvData' carries the decoded 'BS.ByteString' so renders can show
--     real fingerprints, not the @[binary data]@ placeholder we used to
--     emit.
--   * 'PvDate' keeps the raw ISO-8601 text — promoting to 'UTCTime' is
--     a separate concern.
--
-- We deliberately do not derive 'Ord': 'Double' makes it partial via
-- 'NaN', and we don't compare values ordinally anywhere in the
-- codebase.
data PrefValue
  = PvString  !Text
  | PvBool    !Bool
  | PvInteger !Integer
  | PvReal    !Double
  | PvDate    !Text
  | PvData    !BS.ByteString
  | PvArray   ![PrefValue]
  | PvDict    ![(Text, PrefValue)]
  deriving Show

-- | Order-insensitive equality on 'PvDict' so two dicts with the same
-- key/value pairs compare equal regardless of source order. All other
-- constructors compare structurally.
--
-- Note on duplicate keys: 'Defaults.Plist.parsePlist' rejects any dict
-- with duplicate keys ('PlistDuplicateKey'), so values flowing through
-- the normal parsing path can't have duplicates. If a 'PvDict' is
-- constructed directly with duplicates, the 'Eq' instance above
-- compares the sorted list-of-pairs as-is rather than collapsing to
-- last-wins dictionary semantics. That's acceptable because direct
-- construction is internal-only — but worth knowing if you're writing
-- a test that builds 'PvDict' literals.
instance Eq PrefValue where
  PvString a  == PvString b  = a == b
  PvBool a    == PvBool b    = a == b
  PvInteger a == PvInteger b = a == b
  PvReal a    == PvReal b    = a == b
  PvDate a    == PvDate b    = a == b
  PvData a    == PvData b    = a == b
  PvArray a   == PvArray b   = a == b
  PvDict a    == PvDict b    = sortOn fst a == sortOn fst b
  _           == _           = False

-- | Parse-time errors from 'Defaults.Plist.parsePlist'. Carried back
-- by 'Either' rather than thrown so the parser is pure and trivially
-- testable.
data PlistError
  = PlistXmlInvalid Text
    -- ^ The byte stream isn't well-formed XML.
  | PlistShapeInvalid Text
    -- ^ The XML is well-formed but doesn't match the plist 1.0 schema
    -- (e.g. missing @\<plist\>@ root, malformed @\<dict\>@ children).
  | PlistBadInteger Text
  | PlistBadReal Text
  | PlistBadBase64 Text
  | PlistDuplicateKey Text
  deriving (Eq, Show)

-- | Human-readable rendering of a 'PlistError' for the CLI top-level
-- error handler. Avoids exposing constructor names (the derived 'Show'
-- would produce e.g. @PlistBadInteger "abc"@; this produces e.g.
-- @couldn't parse "abc" as an integer@).
renderPlistError :: PlistError -> Text
renderPlistError = \case
  PlistXmlInvalid msg    -> "XML parse error: " <> msg
  PlistShapeInvalid msg  -> "plist structure error: " <> msg
  PlistBadInteger txt    -> "couldn't parse " <> quote txt <> " as an integer"
  PlistBadReal txt       -> "couldn't parse " <> quote txt <> " as a real number"
  PlistBadBase64 msg     -> "invalid base64 in <data>: " <> msg
  PlistDuplicateKey k    -> "duplicate key in <dict>: " <> quote k
  where
    quote t = "\"" <> T.replace "\"" "\\\"" t <> "\""

-- | Representation of the settings of a domain.
newtype Domain = Domain (Map Key PrefValue) deriving (Eq, Show)

-- | Map of domains.
newtype Domains = Domains (Map DomainName Domain) deriving (Eq, Show)

-- | Per-key change between two snapshots of a domain. We never produce
-- a "same value" entry: 'diffDomain' uses 'Data.Map.Merge.Strict.merge'
-- to skip equal values at construction time.
data Delta a
  = Delta !a !a  -- ^ key in both snapshots, values differ
  | Old   !a     -- ^ key only in old snapshot (removed)
  | New   !a     -- ^ key only in new snapshot (added)
  deriving (Eq, Show)

-- | Map representing the change of the values of keys of a domain.
newtype DomainDiff = DomainDiff (Map Key (Delta PrefValue)) deriving (Eq, Show)

-- | A renderable watch-loop event for one domain.
data WatchEvent
  = EventChanged DomainDiff
    -- ^ existing domain whose keys changed
  | EventAdded   [Key] Int
    -- ^ domain that newly appeared post-startup. The keys are the
    -- post-filter list to display; the 'Int' is the total number of
    -- keys before filtering, so the renderer can honestly show
    -- "3 of 200 keys" when most were filtered out.
  | EventRemoved [Key] Int
    -- ^ domain that disappeared from @defaults domains@ mid-watch. Same
    -- shape as 'EventAdded', but the keys are used only for counting
    -- (the renderer prints "(Domain removed, N keys)" and does not
    -- list them). Fires once on first vanish; subsequent polls where
    -- the domain stays missing are silent.
