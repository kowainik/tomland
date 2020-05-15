module Test.Toml.Codec
    ( codecSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Toml.Codec.BiMap (biMapSpec)
import Test.Toml.Codec.Combinators (combinatorsSpec)
import Test.Toml.Codec.Generic (genericSpec)


codecSpec :: Spec
codecSpec = describe "Codec: unit and property tests for bidirectional codecs" $ do
    biMapSpec
    combinatorsSpec
    genericSpec
