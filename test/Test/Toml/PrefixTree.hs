module Test.Toml.PrefixTree
       ( prefixTreeTests
       ) where

import Test.Tasty (TestTree, testGroup)

import Test.Toml.PrefixTree.Property (propertyTests)
import Test.Toml.PrefixTree.Unit (unitTests)

prefixTreeTests :: IO TestTree
prefixTreeTests = do
   uTests <- unitTests
   pure $ testGroup "Prefix Tree tests" $ uTests : propertyTests
