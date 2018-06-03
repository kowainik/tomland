module Test.Toml.Parsing.Property where

import Hedgehog (forAll, (===))

import Toml.Parser (parse)
import Toml.Printer (prettyToml)

import Test.Toml.Gen (PropertyTest, genToml, prop)

test_tomlRoundtrip :: PropertyTest
test_tomlRoundtrip =  prop "parse . prettyPrint == id" $ do
    toml <- forAll genToml
    parse (prettyToml toml) === Right toml
