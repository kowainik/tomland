module Test.Toml.Codec.Combinator.Tuple
    ( tupleSpec
    ) where

import Control.Applicative (liftA2, liftA3)
import Test.Hspec (Spec, describe, parallel)

import Test.Toml.Codec.Combinator.Common (codecRoundtrip)

import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec.Combinator.Primitive as Toml
import qualified Toml.Codec.Combinator.Tuple as Toml


tupleSpec :: Spec
tupleSpec = parallel $ describe "Combinator.Tuple: Roundtrip tests" $ do
    codecRoundtrip
        "(Int, Text)      "
        (const $ Toml.pair (Toml.int "a") (Toml.text "b"))
        (liftA2 (,) Gen.genInt Gen.genText)
    codecRoundtrip
        "(Int, Text, Bool)"
        (const $ Toml.triple (Toml.int "a") (Toml.text "b") (Toml.bool "c"))
        (liftA3 (,,) Gen.genInt Gen.genText Gen.genBool)
