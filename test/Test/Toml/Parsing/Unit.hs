module Test.Toml.Parsing.Unit where

import Test.Tasty.Hspec (Spec)

import Test.Toml.Parsing.Unit.Array (arraySpecs)
import Test.Toml.Parsing.Unit.Bool (boolSpecs)
import Test.Toml.Parsing.Unit.Date (dateSpecs)
import Test.Toml.Parsing.Unit.Double (doubleSpecs)
import Test.Toml.Parsing.Unit.Integer (integerSpecs)
import Test.Toml.Parsing.Unit.Key (keySpecs)
import Test.Toml.Parsing.Unit.Text (textSpecs)
import Test.Toml.Parsing.Unit.Toml (tomlSpecs)

spec_Parser :: Spec
spec_Parser = do
    arraySpecs
    boolSpecs
    dateSpecs
    doubleSpecs
    integerSpecs
    keySpecs
    textSpecs
    tomlSpecs
