module Main (main) where

import qualified PlistSpec
import qualified PrettySpec

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "prefmanager"
  [ PlistSpec.tests
  , PrettySpec.tests
  ]
