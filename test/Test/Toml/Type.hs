module Test.Toml.Type
    ( typeSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Toml.Type.Key (keySpec)
import Test.Toml.Type.PrefixTree (prefixTreeSpec)
import Test.Toml.Type.Printer (printerSpec)
import Test.Toml.Type.TOML (tomlSpec)


typeSpec :: Spec
typeSpec = describe "Toml.Type tests" $ do
    keySpec
    prefixTreeSpec
    printerSpec
    tomlSpec
