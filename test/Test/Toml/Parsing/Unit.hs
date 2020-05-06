module Test.Toml.Parsing.Unit
    ( parsingUnitSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Toml.Parsing.Unit.Array (arraySpecs)
import Test.Toml.Parsing.Unit.Bool (boolSpecs)
import Test.Toml.Parsing.Unit.Date (dateSpecs)
import Test.Toml.Parsing.Unit.Double (doubleSpecs)
import Test.Toml.Parsing.Unit.Integer (integerSpecs)
import Test.Toml.Parsing.Unit.Key (keySpecs)
import Test.Toml.Parsing.Unit.Text (textSpecs)
import Test.Toml.Parsing.Unit.Toml (tomlSpecs)


parsingUnitSpec :: Spec
parsingUnitSpec = describe "Parser Unit tests" $ do
    arraySpecs
    boolSpecs
    dateSpecs
    doubleSpecs
    integerSpecs
    keySpecs
    textSpecs
    tomlSpecs
