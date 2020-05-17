module Test.Toml.Codec
    ( codecSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Toml.Codec.BiMap (biMapSpec)
import Test.Toml.Codec.Combinator (combinatorSpec)
import Test.Toml.Codec.Di (diSpec)
import Test.Toml.Codec.Generic (genericSpec)
import Test.Toml.Codec.SmallType (smallTypeSpec)


codecSpec :: Spec
codecSpec = describe "Codec: unit and property tests for bidirectional codecs" $ do
    biMapSpec
    combinatorSpec
    diSpec
    genericSpec
    smallTypeSpec
