module Test.Toml.Parsing
       ( parsingTests
       ) where

import Test.Tasty (TestTree, testGroup)

import Test.Toml.Parsing.Property (propertyTests)

parsingTests :: TestTree
parsingTests = testGroup "Parsing tests"  propertyTests
