module Main where

import Test.Tasty (defaultMain)

import Test.Toml.PrefixTree (prefixTreeTests)

main :: IO ()
main = prefixTreeTests >>= defaultMain
