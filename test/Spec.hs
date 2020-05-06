module Main (main) where

import Test.Hspec (hspec)
import Test.Hspec.Hedgehog (modifyMaxDiscardRatio)

import Test.Toml.BiCode.Property (biCodePropertySpec)
import Test.Toml.BiMap.Property (biMapPropertySpec)
import Test.Toml.Parsing.Examples (parsingExamplesSpec)
import Test.Toml.Parsing.Property (parsingPropertySpec)
import Test.Toml.Parsing.Unit (parsingUnitSpec)
import Test.Toml.PrefixTree.Property (prefixTreePropertySpec)
import Test.Toml.PrefixTree.Unit (prefixTreeUnitSpec)
import Test.Toml.Printer.Golden (prettyPrinterGoldenSpec)
import Test.Toml.TOML.Property (tomlLawsSpec)


{- Default QuickCheck discard Ratio is 10 while @hedgehog@s is 100.
So we need to modify it manually in here.

See issue: <https://github.com/parsonsmatt/hspec-hedgehog/issues/9>
-}
main :: IO ()
main = hspec $ modifyMaxDiscardRatio (+ 90) $ do
    biCodePropertySpec
    biMapPropertySpec
    -- parsing
    parsingExamplesSpec
    parsingPropertySpec
    parsingUnitSpec
    -- prefix tree
    prefixTreePropertySpec
    prefixTreeUnitSpec
    -- Printer
    prettyPrinterGoldenSpec
    -- toml
    tomlLawsSpec
