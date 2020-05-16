{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Toml.Codec.BiMap
    ( biMapSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Toml.Codec.BiMap.Conversion (conversionSpec)


biMapSpec :: Spec
biMapSpec = describe "Tagged Partial Bidirectional Isomorphism: tests" $
    conversionSpec
