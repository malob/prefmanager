{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Defaults.Pretty where

import Defaults.Types
  (Delta(..), Domain(..), DomainDiff(..), DomainName(..), Key, PrefValue(..), WatchEvent(..))

import Prelude hiding (group)
import Relude.Extra (un)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Map.Strict (foldMapWithKey)
import Prettyprinter
import Prettyprinter.Render.Terminal

-- | Render the events produced by one watch-loop iteration. Each domain
-- gets one block: a per-key diff for changed domains, a key list with
-- count for newly-appeared domains.
prettyWatchEvents :: Map DomainName WatchEvent -> Doc AnsiStyle
prettyWatchEvents = foldMapWithKey go
  where
    go (DomainName name) = \case
      EventChanged dd ->
        annotate (bold <> italicized) (pretty name)
        <> hardline <> hardline
        <> indent 2 (prettyDomainDiff dd)
        <> hardline
      EventAdded ks total ->
        annotate (bold <> italicized) (pretty name)
        <+> "(Domain added," <+> renderCount ks total <> ")"
        <> hardline <> hardline
        <> indent 2 (vsep (pretty <$> ks))
        <> hardline <> hardline
      EventRemoved ks total ->
        annotate (bold <> italicized) (pretty name)
        <+> "(Domain removed," <+> renderCount ks total <> ")"
        <> hardline <> hardline

renderCount :: [a] -> Int -> Doc AnsiStyle
renderCount ks total =
  let n = length ks
      filtered = total - n
  in if filtered == 0
       then pretty n <+> if n == 1 then "key" else "keys"
       else pretty n <+> "of" <+> pretty total <+> "keys,"
            <+> pretty filtered <+> "filtered"

prettyDomainDiffs :: Map DomainName DomainDiff -> Doc AnsiStyle
prettyDomainDiffs = foldMapWithKey go where
  go :: DomainName -> DomainDiff -> Doc AnsiStyle
  go (DomainName name) diff
    = annotate (bold <> italicized) (pretty name)
    <> hardline
    <> hardline
    <> indent 2 (prettyDomainDiff diff)
    <> hardline

prettyDomainDiff :: DomainDiff -> Doc AnsiStyle
prettyDomainDiff = foldMapWithKey go . un where
  go :: Key -> Delta PrefValue -> Doc AnsiStyle
  go key = (<> hardline <> hardline) . \case
    Delta old new
      -> pretty key <+> "(Value changed)"
      <> hardline
      <> indent 2 (red (pretty old) <> hardline <> green (pretty new))
    New x
      -> green
      $  pretty key <+> "(Key added)"
      <> hardline
      <> indent 2 (pretty x)
    Old x
      -> red
      $  pretty key <+> "(Key removed)"
      <> hardline
      <> indent 2 (pretty x)
  red = annotate $ colorDull Red
  green = annotate $ colorDull Green

-- | Render a 'PrefValue' as the same XML-shaped output the previous
-- 'Pretty PlObject' instance produced — keeping the
-- copy-pasteable-into-@defaults write@ affinity that motivated the
-- format. Dict entries render in source order ('PvDict' is a
-- list-of-pairs); string content preserves newlines via 'hardline'
-- which 'group' can't flatten. 'PvData' renders a length and a
-- deterministic base64 fingerprint sampling the start and end of the
-- encoded form so changes anywhere in the blob are visible.
instance Pretty PrefValue where
  pretty = \case
    PvString  x -> tag "string" $ verbatim x
    PvBool    x -> if x then "<true/>" else "<false/>"
    PvInteger x -> tag "integer" $ pretty x
    PvReal    x -> tag "real" $ pretty x
    PvArray   x -> tag "array" $ concatWith ((<>) . (<> hardline)) (fmap pretty x)
    PvDict    x -> tag "dict" $ concatWith ((<>) . (<> hardline)) (prettyDict <$> x)
    PvData    x -> tag "data" (prettyData x)
    PvDate    x -> tag "date" $ pretty x

    where
      prettyDict (k, o) = tag "key" (pretty k) <> hardline <> pretty o
      tag t s = group $ flatAlt
        (open <> hardline <> indent 4 s <> hardline <> close)
        (open <> s <> close)
        where
          open = angles t
          close = angles ("/" <> t)

-- | Render a 'Text' preserving its line structure. @prettyprinter@'s
-- standard 'Pretty Text' instance produces 'line' breaks that 'group'
-- can flatten to spaces; for @\<string\>@ content we use 'hardline'
-- so a parsed @"foo\\nbar"@ stays multi-line in the output instead of
-- rendering as @"foo bar"@.
verbatim :: Text -> Doc ann
verbatim = concatWith (\a b -> a <> hardline <> b) . fmap pretty . T.lines

-- | Render a 'PvData' payload as @[N bytes, base64:AAAAAAAA…ZZZZZZZZ]@.
-- The fingerprint samples the first and last 8 base64 characters of
-- the encoded form so changes anywhere in the blob have a strong
-- chance of being visible — covers ~12 raw bytes (6 prefix + 6 suffix)
-- which is enough to discriminate typical macOS plist data values.
-- Prefix-only and 4+4 samples both leave middle-only changes invisible
-- for medium-length blobs; 8+8 is a noticeable improvement at no cost
-- in display width. For small blobs (≤ 24 base64 chars) we emit the
-- whole encoded form rather than truncate.
prettyData :: BS.ByteString -> Doc ann
prettyData bs = pretty msg
  where
    n        = BS.length bs
    encoded  = B64.encode bs
    fp
      | BS.length encoded <= 24 = TE.decodeLatin1 encoded
      | otherwise = TE.decodeLatin1 (BS.take 8 encoded)
                 <> "\x2026"  -- "…"
                 <> TE.decodeLatin1 (BS.drop (BS.length encoded - 8) encoded)
    unit     = if n == 1 then "byte" else "bytes" :: Text
    msg      = "[" <> show n <> " " <> unit <> ", base64:" <> fp <> "]" :: Text
