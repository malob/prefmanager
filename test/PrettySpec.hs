{-# LANGUAGE OverloadedStrings #-}
module PrettySpec (tests) where

import Defaults.Pretty ()  -- the Pretty PrefValue instance
import Defaults.Types (PrefValue(..))

import qualified Data.Text as T
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as PRT

import Prettyprinter (Doc, layoutPretty, defaultLayoutOptions, unAnnotate)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

-- | Strip ANSI annotations and render to plain Text for testing.
render :: Doc AnsiStyle -> Text
render = PRT.renderStrict . layoutPretty defaultLayoutOptions . unAnnotate

tests :: TestTree
tests = testGroup "Defaults.Pretty"
  [ testCase "PvDict renders keys in source order, not alphabetical" $ do
      -- Build a dict whose source order is z-then-a. The renderer
      -- should emit <key>z</key> before <key>a</key>; with a Map-based
      -- representation it would be the other way around.
      let d = PvDict [("z", PvInteger 1), ("a", PvInteger 2)]
          out = render (P.pretty d)
          zPos = T.length . fst $ T.breakOn "<key>z</key>" out
          aPos = T.length . fst $ T.breakOn "<key>a</key>" out
      assertBool ("source order preserved; got:\n" <> T.unpack out) (zPos < aPos)

  , testCase "nested PvDict also preserves source order" $ do
      let d = PvDict [("outer", PvDict [("z", PvBool True), ("a", PvBool False)])]
          out = render (P.pretty d)
          zPos = T.length . fst $ T.breakOn "<key>z</key>" out
          aPos = T.length . fst $ T.breakOn "<key>a</key>" out
      assertBool ("nested source order preserved; got:\n" <> T.unpack out) (zPos < aPos)
  ]
