module Test.Toml.Parser.Property
    ( propertySpec
    ) where

import Hedgehog (forAll, tripping)
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.Gen (genToml)
import Toml.Parser (parse)
import Toml.Type.Printer (pretty)


propertySpec :: Spec
propertySpec = describe "Parsing property tests"
    parsePrintRoundTrip

parsePrintRoundTrip :: SpecWith (Arg Expectation)
parsePrintRoundTrip = it "parse . prettyPrint == id" $ hedgehog $ do
    toml <- forAll genToml
    tripping toml pretty parse
