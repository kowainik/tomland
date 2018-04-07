module Main where

import Test.Tasty (defaultMain, testGroup)

import Test.Toml.Parsing (parsingTests)
import Test.Toml.PrefixTree (prefixTreeTests)

main :: IO ()
main = do
    prefixTreeTest <- prefixTreeTests
    defaultMain $ testGroup "Tomland tests" [parsingTests, prefixTreeTest]
