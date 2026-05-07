{-# LANGUAGE OverloadedStrings #-}
module PlistSpec (tests) where

import Defaults.Plist (parsePlist)
import Defaults.Types (PlistError(..), PrefValue(..))

import qualified Data.ByteString.Lazy as LBS

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

tests :: TestTree
tests = testGroup "Defaults.Plist"
  [ scalars
  , dataElement
  , collections
  , malformed
  , dictSemantics
  , whitespaceDiscipline
  , realWorld
  ]

-- Wrap a body in the @<plist version="1.0">…</plist>@ document we expect.
plistDoc :: LBS.ByteString -> LBS.ByteString
plistDoc body = LBS.concat
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
  , "<plist version=\"1.0\">", body, "</plist>"
  ]

scalars :: TestTree
scalars = testGroup "scalars"
  [ testCase "string" $
      parsePlist (plistDoc "<string>hello</string>") @?= Right (PvString "hello")
  , testCase "true" $
      parsePlist (plistDoc "<true/>") @?= Right (PvBool True)
  , testCase "false" $
      parsePlist (plistDoc "<false/>") @?= Right (PvBool False)
  , testCase "integer" $
      parsePlist (plistDoc "<integer>42</integer>") @?= Right (PvInteger 42)
  , testCase "negative integer" $
      parsePlist (plistDoc "<integer>-7</integer>") @?= Right (PvInteger (-7))
  , testCase "very large integer (beyond Int64)" $
      parsePlist (plistDoc "<integer>123456789012345678901234567890</integer>")
        @?= Right (PvInteger 123456789012345678901234567890)
  , testCase "real" $
      parsePlist (plistDoc "<real>3.14</real>") @?= Right (PvReal 3.14)
  , testCase "date kept as text" $
      parsePlist (plistDoc "<date>2024-01-15T10:00:00Z</date>")
        @?= Right (PvDate "2024-01-15T10:00:00Z")
  , testCase "string preserves internal whitespace" $
      parsePlist (plistDoc "<string>  hi  </string>")
        @?= Right (PvString "  hi  ")
  , testCase "string preserves embedded newlines" $
      parsePlist (plistDoc "<string>line1\nline2</string>")
        @?= Right (PvString "line1\nline2")
  ]

dataElement :: TestTree
dataElement = testGroup "<data>"
  [ testCase "round-trip with no whitespace" $
      parsePlist (plistDoc "<data>3q2+7w==</data>")
        @?= Right (PvData "\xDE\xAD\xBE\xEF")
  , testCase "round-trip with whitespace (the upstream bug)" $
      -- This is the exact shape `defaults export` emits and what the
      -- old parser silently dropped.
      parsePlist (plistDoc "<dict><key>blob</key><data>\n\t3q2+7w==\n\t</data></dict>")
        @?= Right (PvDict [("blob", PvData "\xDE\xAD\xBE\xEF")])
  , testCase "different blobs decode differently" $ do
      let a = parsePlist (plistDoc "<data>3q2+7w==</data>")
          b = parsePlist (plistDoc "<data>yv66vg==</data>")
      assertBool "should differ" (a /= b)
  , testCase "invalid base64 surfaces PlistBadBase64" $
      case parsePlist (plistDoc "<data>not-base64!!!</data>") of
        Left (PlistBadBase64 _) -> pure ()
        other -> assertEqual "expected PlistBadBase64" (Left (PlistBadBase64 "_")) other
  ]

collections :: TestTree
collections = testGroup "collections"
  [ testCase "empty array" $
      parsePlist (plistDoc "<array></array>") @?= Right (PvArray [])
  , testCase "array of strings" $
      parsePlist (plistDoc "<array><string>a</string><string>b</string></array>")
        @?= Right (PvArray [PvString "a", PvString "b"])
  , testCase "empty dict" $
      parsePlist (plistDoc "<dict></dict>") @?= Right (PvDict [])
  , testCase "dict of mixed scalars" $
      parsePlist (plistDoc
        "<dict><key>n</key><integer>1</integer><key>s</key><string>foo</string></dict>")
        @?= Right (PvDict [("n", PvInteger 1), ("s", PvString "foo")])
  , testCase "deeply nested: dict-of-arrays-of-dicts" $
      parsePlist (plistDoc
        "<dict>\
        \<key>xs</key>\
        \<array>\
          \<dict><key>k</key><string>v</string></dict>\
          \<integer>2</integer>\
        \</array>\
        \</dict>")
        @?= Right (PvDict [("xs",
              PvArray [ PvDict [("k", PvString "v")]
                      , PvInteger 2 ])])
  ]

malformed :: TestTree
malformed = testGroup "malformed input"
  [ testCase "non-XML bytes" $
      case parsePlist "not xml" of
        Left (PlistXmlInvalid _) -> pure ()
        other -> assertEqual "expected PlistXmlInvalid" (Left (PlistXmlInvalid "_")) other
  , testCase "missing <plist> root" $
      case parsePlist "<?xml version=\"1.0\"?><dict></dict>" of
        Left (PlistShapeInvalid _) -> pure ()
        other -> assertEqual "expected PlistShapeInvalid" (Left (PlistShapeInvalid "_")) other
  , testCase "bad integer" $
      case parsePlist (plistDoc "<integer>abc</integer>") of
        Left (PlistBadInteger _) -> pure ()
        other -> assertEqual "expected PlistBadInteger" (Left (PlistBadInteger "_")) other
  , testCase "bad real" $
      case parsePlist (plistDoc "<real>not-a-number</real>") of
        Left (PlistBadReal _) -> pure ()
        other -> assertEqual "expected PlistBadReal" (Left (PlistBadReal "_")) other
  , testCase "<dict> with <key> but no value" $
      case parsePlist (plistDoc "<dict><key>k</key></dict>") of
        Left (PlistShapeInvalid _) -> pure ()
        other -> assertEqual "expected PlistShapeInvalid" (Left (PlistShapeInvalid "_")) other
  , testCase "<dict> child not <key>" $
      case parsePlist (plistDoc "<dict><string>oops</string><integer>1</integer></dict>") of
        Left (PlistShapeInvalid _) -> pure ()
        other -> assertEqual "expected PlistShapeInvalid" (Left (PlistShapeInvalid "_")) other
  , testCase "non-whitespace text between collection children rejected" $
      -- The parser used to silently drop this junk text and return a
      -- valid one-key dict. It now errors loudly.
      case parsePlist (plistDoc "<dict>junk<key>k</key><string>v</string></dict>") of
        Left (PlistShapeInvalid _) -> pure ()
        other -> assertEqual "expected PlistShapeInvalid" (Left (PlistShapeInvalid "_")) other
  , testCase "non-whitespace text between array children rejected" $
      case parsePlist (plistDoc "<array><string>a</string>junk<string>b</string></array>") of
        Left (PlistShapeInvalid _) -> pure ()
        other -> assertEqual "expected PlistShapeInvalid" (Left (PlistShapeInvalid "_")) other
  ]

dictSemantics :: TestTree
dictSemantics = testGroup "dict semantics"
  [ testCase "duplicate keys rejected" $
      case parsePlist (plistDoc
        "<dict><key>k</key><integer>1</integer><key>k</key><integer>2</integer></dict>") of
        Left (PlistDuplicateKey "k") -> pure ()
        other -> assertEqual "expected PlistDuplicateKey \"k\"" (Left (PlistDuplicateKey "k")) other
  , testCase "key order in source doesn't affect equality" $ do
      let a = parsePlist (plistDoc
            "<dict><key>x</key><integer>1</integer><key>y</key><integer>2</integer></dict>")
          b = parsePlist (plistDoc
            "<dict><key>y</key><integer>2</integer><key>x</key><integer>1</integer></dict>")
      assertEqual "same dict regardless of source order" a b
  , testCase "PvDict preserves source order in its list representation" $
      -- Same content, different source order: equal under our 'Eq', but
      -- the underlying list-of-pairs is structurally distinct. This is
      -- the "faithful render / semantic compare" split made concrete.
      case parsePlist (plistDoc "<dict><key>z</key><integer>1</integer><key>a</key><integer>2</integer></dict>") of
        Right (PvDict pairs) ->
          assertEqual "source order preserved in list" [("z", PvInteger 1), ("a", PvInteger 2)] pairs
        other -> assertEqual "expected PvDict" (Right (PvDict [])) other
  ]

whitespaceDiscipline :: TestTree
whitespaceDiscipline = testGroup "whitespace discipline"
  [ testCase "<integer> tolerates surrounding whitespace" $
      parsePlist (plistDoc "<integer>\n  42\n  </integer>")
        @?= Right (PvInteger 42)
  , testCase "<date> tolerates surrounding whitespace" $
      parsePlist (plistDoc "<date>\n  2024-01-15T10:00:00Z\n  </date>")
        @?= Right (PvDate "2024-01-15T10:00:00Z")
  , testCase "indented dict (shape `defaults export` emits)" $
      parsePlist (plistDoc
        "\n  <dict>\n    <key>foo</key>\n    <string>bar</string>\n  </dict>\n")
        @?= Right (PvDict [("foo", PvString "bar")])
  ]

realWorld :: TestTree
realWorld = testGroup "real-world shapes"
  [ testCase "defaults-export style with leading whitespace and newlines" $
      parsePlist (plistDoc
        "\n\t<dict>\n\t\t<key>foo</key>\n\t\t<string>bar</string>\n\t</dict>\n")
        @?= Right (PvDict [("foo", PvString "bar")])
  , testCase "domain with binary data — the regression that motivated this rewrite" $
      parsePlist (plistDoc
        "\n<dict>\n\t<key>blob</key>\n\t<data>\n\t3q2+7w==\n\t</data>\n</dict>\n")
        @?= Right (PvDict [("blob", PvData "\xDE\xAD\xBE\xEF")])
  ]
