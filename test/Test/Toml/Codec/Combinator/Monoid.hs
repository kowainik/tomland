module Test.Toml.Codec.Combinator.Monoid
    ( monoidSpec
    ) where

import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Test.Hspec (Spec, describe)

import Test.Toml.Codec.Combinator.Common (codecRoundtrip)

import qualified Hedgehog.Gen as Gen

import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec.Combinator.Monoid as Toml
import qualified Toml.Codec.Combinator.Primitive as Toml


monoidSpec :: Spec
monoidSpec = describe "Combinator.Monoid: Roundtrip tests" $ do
    codecRoundtrip "All        " Toml.all                (All     <$> Gen.genBool)
    codecRoundtrip "Any        " Toml.any                (Any     <$> Gen.genBool)
    codecRoundtrip "Sum     Int" (Toml.sum Toml.int)     (Sum     <$> Gen.genInt)
    codecRoundtrip "Product Int" (Toml.product Toml.int) (Product <$> Gen.genInt)
    codecRoundtrip "First   Int" (Toml.first Toml.int)   (First   <$> Gen.maybe Gen.genInt)
    codecRoundtrip "Last    Int" (Toml.last Toml.int)    (Last    <$> Gen.maybe Gen.genInt)
