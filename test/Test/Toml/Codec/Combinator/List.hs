module Test.Toml.Codec.Combinator.List
    ( listSpec
    ) where

import Test.Hspec (Spec, describe, parallel)

import Test.Toml.Codec.Combinator.Common (codecRoundtrip)

import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec.BiMap.Conversion as Toml
import qualified Toml.Codec.Combinator.List as Toml
import qualified Toml.Codec.Combinator.Primitive as Toml


listSpec :: Spec
listSpec = parallel $ describe "Combinator.List: Roundtrip tests" $ do
    codecRoundtrip "[Int] (Array)       "
        (Toml.arrayOf Toml._Int)
        (Gen.genList Gen.genInt)
    codecRoundtrip "NonEmpty Int (Array)"
        (Toml.arrayNonEmptyOf Toml._Int)
        (Gen.genNonEmpty Gen.genInt)
    codecRoundtrip "[Int] (Table)       "
        (Toml.list $ Toml.int "a")
        (Gen.genList Gen.genInt)
    codecRoundtrip "NonEmpty Int (Table)"
        (Toml.nonEmpty $ Toml.int "a")
        (Gen.genNonEmpty Gen.genInt)
