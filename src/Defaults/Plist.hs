{-# LANGUAGE OverloadedStrings #-}
-- | Pure plist 1.0 parser scoped to the subset @/usr/bin/defaults
-- export@ emits.
--
-- This module replaces the @plist@ + @hxt@ pair we used to depend on
-- and fixes a long-standing parsing bug: upstream @plist@ silently
-- dropped any @\<key\>\<data\>@ pair where the @\<data\>@ element
-- contained leading or trailing whitespace, which is exactly what
-- @defaults export@ always emits. As a result, binary preference
-- values were invisible to prefmanager. Owning the parser lets us
-- strip whitespace where the plist grammar permits and keep it where
-- it doesn't (notably, inside @\<string\>@ content).
module Defaults.Plist
  ( parsePlist
  ) where

import Defaults.Types (PlistError(..), PrefValue(..))

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.XML as X

import Text.Read (readMaybe)

-- | Parse a @\<plist version=\"1.0\"\>@ XML document into a 'PrefValue'.
-- Returns @Left@ on malformed XML, schema mismatches, or content that
-- doesn't fit the corresponding constructor (bad base64, unparseable
-- numbers, duplicate dict keys, etc.).
parsePlist :: LBS.ByteString -> Either PlistError PrefValue
parsePlist bs = case X.parseLBS X.def bs of
  Left e -> Left (PlistXmlInvalid (T.pack (show e)))
  Right doc ->
    let root = X.documentRoot doc
    in if X.nameLocalName (X.elementName root) /= "plist"
         then Left (PlistShapeInvalid "expected <plist> root element")
         else case childElements root of
                [child] -> elementToValue child
                _ -> Left (PlistShapeInvalid "<plist> must contain exactly one child element")

-- Convert one XML element to a 'PrefValue'.
elementToValue :: X.Element -> Either PlistError PrefValue
elementToValue el = case X.nameLocalName (X.elementName el) of
  "string"  -> Right (PvString (elementText el))
  "true"    -> Right (PvBool True)
  "false"   -> Right (PvBool False)
  "integer" -> case readMaybe (T.unpack (T.strip (elementText el))) of
    Just n  -> Right (PvInteger n)
    Nothing -> Left (PlistBadInteger (elementText el))
  "real" -> case readMaybe (T.unpack (T.strip (elementText el))) of
    Just n  -> Right (PvReal n)
    Nothing -> Left (PlistBadReal (elementText el))
  "date" -> Right (PvDate (T.strip (elementText el)))
  "data" -> case B64.decode (TE.encodeUtf8 (T.filter (not . isBase64Whitespace) (elementText el))) of
    Right bytes -> Right (PvData bytes)
    Left err    -> Left (PlistBadBase64 (T.pack err))
  "array" -> do
    children <- requireOnlyElements el
    PvArray <$> traverse elementToValue children
  "dict" -> do
    children <- requireOnlyElements el
    dictFromChildren children
  other -> Left (PlistShapeInvalid ("unknown plist element: " <> other))

-- | Pair up @\<key\>@/value children and assemble a 'PvDict'. Errors on
-- duplicate keys or odd-length sequences (@\<key\>@ without a sibling).
-- Preserves source order so renderers can faithfully show the dict the
-- way the underlying plist file laid it out.
dictFromChildren :: [X.Element] -> Either PlistError PrefValue
dictFromChildren = go Set.empty []
  where
    go _    acc []           = Right (PvDict (reverse acc))
    go seen acc (k : v : rest)
      | X.nameLocalName (X.elementName k) /= "key" =
          Left (PlistShapeInvalid "expected <key> in <dict>")
      | otherwise = do
          let keyText = T.strip (elementText k)
          when' (Set.member keyText seen) $
            Left (PlistDuplicateKey keyText)
          val <- elementToValue v
          go (Set.insert keyText seen) ((keyText, val) : acc) rest
    go _ _ [_] =
      Left (PlistShapeInvalid "<dict> has odd number of children")

    when' True  e = e
    when' False _ = Right ()

-- | Concatenated text content of an element (skipping nested elements).
-- Used for scalar leaves like @\<string\>@, @\<integer\>@, etc.
elementText :: X.Element -> Text
elementText el = T.concat [ t | X.NodeContent t <- X.elementNodes el ]

-- | Direct element children of an element. Errors if there is any
-- non-whitespace text node mixed in among the children, since plist
-- collections (@\<array\>@, @\<dict\>@) don't allow mixed content. This
-- catches malformed inputs that the original parser silently dropped.
requireOnlyElements :: X.Element -> Either PlistError [X.Element]
requireOnlyElements el = go [] (X.elementNodes el)
  where
    go acc []                             = Right (reverse acc)
    go acc (X.NodeElement child : rest)   = go (child : acc) rest
    go acc (X.NodeContent t : rest)
      | T.all isXmlWhitespace t           = go acc rest
      | otherwise = Left (PlistShapeInvalid
          ("unexpected text content in <" <> tagName <> ">: "
           <> T.take 40 (T.strip t)))
    go acc (_ : rest)                     = go acc rest  -- skip comments / instructions
    tagName = X.nameLocalName (X.elementName el)

-- | Direct element children, used at the top level (where text between
-- elements is fine because xml-conduit gives us only the @<plist>@
-- wrapper to inspect).
childElements :: X.Element -> [X.Element]
childElements el = [ child | X.NodeElement child <- X.elementNodes el ]

-- | Whitespace recognized inside XML element content (XML 1.0 §2.10):
-- space, tab, CR, LF. Matches what we want to ignore between collection
-- children — narrower than 'Data.Char.isSpace' which accepts NBSP and
-- other Unicode whitespace.
isXmlWhitespace :: Char -> Bool
isXmlWhitespace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | Whitespace tolerated inside @\<data\>@ content. Same set as
-- 'isXmlWhitespace': base64 padding shouldn't include exotic
-- whitespace, and using 'Data.Char.isSpace' would silently strip
-- characters like NBSP that should fail base64 decoding.
isBase64Whitespace :: Char -> Bool
isBase64Whitespace = isXmlWhitespace
