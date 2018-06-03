module Test.Toml.Parsing.Property where

import Hedgehog (forAll, tripping)

import Toml.Parser (parse)
import Toml.Printer (prettyToml)

import Test.Toml.Gen (PropertyTest, genToml, prop)

test_tomlRoundtrip :: PropertyTest
test_tomlRoundtrip =  prop "parse . prettyPrint == id" $ do
    toml <- forAll genToml
    tripping toml prettyToml parse
