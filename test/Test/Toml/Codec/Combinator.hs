module Test.Toml.Codec.Combinator
    ( combinatorSpec
    ) where

import Test.Hspec (Spec, describe, parallel)

import Test.Toml.Codec.Combinator.Custom (customSpec)
import Test.Toml.Codec.Combinator.List (listSpec)
import Test.Toml.Codec.Combinator.Map (mapSpec)
import Test.Toml.Codec.Combinator.Monoid (monoidSpec)
import Test.Toml.Codec.Combinator.Primitive (primitiveSpec)
import Test.Toml.Codec.Combinator.Set (setSpec)
import Test.Toml.Codec.Combinator.Table (tableSpec)
import Test.Toml.Codec.Combinator.Time (timeSpec)
import Test.Toml.Codec.Combinator.Tuple (tupleSpec)


combinatorSpec :: Spec
combinatorSpec = parallel $ describe "Combinator spec" $ do
    customSpec
    listSpec
    mapSpec
    monoidSpec
    primitiveSpec
    setSpec
    tableSpec
    timeSpec
    tupleSpec
