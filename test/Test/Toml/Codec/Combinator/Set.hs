module Test.Toml.Codec.Combinator.Set
    ( setSpec
    ) where

import Test.Hspec (Spec, describe, parallel)

import Test.Toml.Codec.Combinator.Common (codecRoundtrip)

import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec.BiMap.Conversion as Toml
import qualified Toml.Codec.Combinator.Primitive as Toml
import qualified Toml.Codec.Combinator.Set as Toml


setSpec :: Spec
setSpec = parallel $ describe "Combinator.Set: Roundtrip tests" $ do
    codecRoundtrip "IntSet (Array)      " Toml.arrayIntSet Gen.genIntSet
    codecRoundtrip "IntSet (Table)      " (Toml.intSet $ Toml.int "a") Gen.genIntSet
    codecRoundtrip "Set Int (Array)     " (Toml.arraySetOf Toml._Int) (Gen.genSet Gen.genInt)
    codecRoundtrip "Set Int (Table)     "
        (Toml.set $ Toml.int "a")
        (Set.fromList <$> Gen.genSmallList Gen.genInt)
    codecRoundtrip "HashSet Text (Array)"
        (Toml.arrayHashSetOf Toml._Text)
        (HashSet.fromList <$> Gen.genSmallList Gen.genText)
    codecRoundtrip "HashSet Text (Table)"
        (Toml.hashSet $ Toml.text "a")
        (HashSet.fromList <$> Gen.genSmallList Gen.genText)
