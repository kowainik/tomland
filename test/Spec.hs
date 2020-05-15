module Main (main) where

import Test.Hspec (hspec)
import Test.Hspec.Hedgehog (modifyMaxDiscardRatio)

import Test.Toml.Codec (codecSpec)
import Test.Toml.Parser (parserSpec)
import Test.Toml.Type (typeSpec)


{- Default QuickCheck discard Ratio is 10 while @hedgehog@s is 100.
So we need to modify it manually in here.

See issue: <https://github.com/parsonsmatt/hspec-hedgehog/issues/9>
-}
main :: IO ()
main = hspec $ modifyMaxDiscardRatio (+ 90) $ do
    typeSpec
    parserSpec
    codecSpec
