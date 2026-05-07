{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Defaults.Pretty where

import Defaults.Types
  (Delta(..), Domain(..), DomainDiff(..), DomainName(..), Key, WatchEvent(..))

import Prelude hiding (group)
import Relude.Extra (un)

import qualified Data.Map.Strict as M
import Data.Map.Strict (foldMapWithKey)
import Prettyprinter
import Prettyprinter.Render.Terminal
import Text.XML.Plist (PlObject(..))

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
  go :: Key -> Delta PlObject -> Doc AnsiStyle
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

instance Pretty PlObject where
  pretty = \case
    PlString  x -> tag "string" $ pretty x
    PlBool    x -> bool "<false/>" "<true/>" x
    PlInteger x -> tag "integer" $ pretty x
    PlReal    x -> tag "real" $ pretty x
    PlArray   x -> tag "array" $ concatWith ((<>) . (<> hardline)) (fmap pretty x)
    PlDict    x -> tag "dict" $ concatWith ((<>) . (<> hardline)) (fmap prettyDict x)
    PlData    x -> tag "data" "[binary data]"
    PlDate    x -> tag "date" $ pretty x

    where
      prettyDict (k, o) = tag "key" (pretty k) <> hardline <> pretty o
      tag t s = group $ flatAlt
        (open <> hardline <> indent 4 s <> hardline <> close)
        (open <> s <> close)
        where
          open = angles t
          close = angles ("/" <> t)

