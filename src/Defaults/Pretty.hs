{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Defaults.Pretty where

import Defaults.Types (DomainDiff(..), DomainName(..), Key)

import Prelude hiding (group)
import Relude.Extra (un)

import Data.Map.Strict (foldMapWithKey)
import Patience.Map (Delta(..))
import Prettyprinter
import Prettyprinter.Render.Terminal
import Text.XML.Plist (PlObject(..))

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
    Same x
      -> pretty key <+> "(No change)"
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

