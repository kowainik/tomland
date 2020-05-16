module Test.Toml.Type.Key
    ( keySpec
    ) where

import Data.String (IsString (..))
import Hedgehog (forAll, tripping)
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.Gen (genKey)

import qualified Data.Text as Text
import qualified Toml.Type.Printer as Printer


keySpec :: Spec
keySpec = describe "TOML Key"
    keyRoundtripSpec

keyRoundtripSpec :: SpecWith (Arg Expectation)
keyRoundtripSpec = it "Key printing: fromString . prettyKey ≡ id" $ hedgehog $ do
    key <- forAll genKey
    tripping key Printer.prettyKey (Just . fromString . Text.unpack)
