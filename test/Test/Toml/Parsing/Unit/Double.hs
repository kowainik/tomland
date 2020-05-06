module Test.Toml.Parsing.Unit.Double
    (doubleSpecs
    ) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (parseSatisfies)
import Text.Megaparsec (parse)

import Test.Toml.Parsing.Unit.Common (doubleFailOn, parseDouble)
import Toml.Parser.Value (doubleP)


doubleSpecs :: Spec
doubleSpecs = describe "doubleP" $ do
    it "can parse a number which consists of an integral part, and a fractional part" $ do
        parseDouble "+1.0"      1.0
        parseDouble "3.1415"    3.1415
        parseDouble "0.0"       0.0
        parseDouble "-0.01"     (-0.01)
        parseDouble "5e+22"     5e+22
        parseDouble "1e6"       1e6
        parseDouble "-2E-2"     (-2E-2)
        parseDouble "6.626e-34" 6.626e-34
    it "can parse a number with underscores" $ do
        parseDouble "5e+2_2"         5e+22
        parseDouble "1.1_1e6"        1.11e6
        parseDouble "-2_2.21_9E-0_2" (-22.219E-2)
    it "can parse sign-prefixed zero" $ do
        parseDouble "+0.0" 0.0
        parseDouble "-0.0" (-0.0)
    it "can parse positive and negative special float values (inf and nan)" $ do
        parseDouble "inf"  (1 / 0)
        parseDouble "+inf" (1 / 0)
        parseDouble "-inf" (-1 / 0)
        doubleSatisfies "nan"  isNaN
        doubleSatisfies "+nan" isNaN
        doubleSatisfies "-nan" isNaN
    it "fails if `inf` or `nan` are not all lowercase" $ do
        doubleFailOn "Inf"
        doubleFailOn "INF"
        doubleFailOn "Nan"
        doubleFailOn "NAN"
        doubleFailOn "NaN"
  where
    doubleSatisfies given f = parse doubleP "" given `parseSatisfies` f
