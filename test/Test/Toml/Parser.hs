module Test.Toml.Parser
    ( parserSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Toml.Parser.Array (arraySpecs)
import Test.Toml.Parser.Bool (boolSpecs)
import Test.Toml.Parser.Date (dateSpecs)
import Test.Toml.Parser.Double (doubleSpecs)
import Test.Toml.Parser.Examples (examplesSpec, parseUnparseSpec)
import Test.Toml.Parser.Integer (integerSpecs)
import Test.Toml.Parser.Key (keySpecs)
import Test.Toml.Parser.Property (propertySpec)
import Test.Toml.Parser.Text (textSpecs)
import Test.Toml.Parser.Toml (tomlSpecs)
import Test.Toml.Parser.Validate (validateSpec)


parserSpec :: Spec
parserSpec = describe "Parser for TOML" $ do
    examplesSpec
    parseUnparseSpec
    propertySpec
    validateSpec

    -- unit tests for different parser parts
    arraySpecs
    boolSpecs
    dateSpecs
    doubleSpecs
    integerSpecs
    keySpecs
    textSpecs
    tomlSpecs
