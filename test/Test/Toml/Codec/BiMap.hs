{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Toml.Codec.BiMap
    ( biMapSpec
    ) where

import Prelude hiding (id, (.))

import Control.Category (id, (.))
import Data.Word (Word8)
import Hedgehog (Gen, PropertyT, forAll, (===))
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.Codec.BiMap.Conversion (conversionSpec)
import Test.Toml.Gen (genInteger, genText, genWord8)
import Toml.Codec.BiMap (BiMap (..), TomlBiMap)
import Toml.Codec.BiMap.Conversion (_BoundedInteger, _ReadString, _StringText)


biMapSpec :: Spec
biMapSpec = parallel $ describe "Tagged Partial Bidirectional Isomorphism: tests" $ do
    conversionSpec
    categoryLaws

categoryLaws :: Spec
categoryLaws = describe "BiMap Category instance laws" $ do
    it "Right identity: f . id ≡ f" $ biMapEquality
        (_Word8Integer . id)
        _Word8Integer
        genWord8
        genInteger

    it "Left identity:  id . f ≡ f" $ biMapEquality
        _Word8Integer
        (_Word8Integer . id)
        genWord8
        genInteger

    it "Associativity:  f . (g . h) ≡ (f . g) . h" $ biMapEquality
        (_StringText . (_ReadString . _Word8Integer))
        ((_StringText . _ReadString) . _Word8Integer)
        genWord8
        genText

{- | Property that takes two 'BiMap's and checks them on equality. We
consider two 'BiMap's @m1@ and @m2@ equal if:

* @forward  m1 ≡ forward  m2@
* @backward m1 ≡ backward m2@
-}
biMapEquality
    :: (Eq a, Eq b, Show a, Show b)
    => TomlBiMap a b
    -> TomlBiMap a b
    -> Gen a
    -> Gen b
    -> PropertyT IO ()
biMapEquality m1 m2 genA genB = hedgehog $ do
    a <- forAll genA
    b <- forAll genB

    forward  m1 a === forward  m2 a
    backward m1 b === backward m2 b

_Word8Integer :: TomlBiMap Word8 Integer
_Word8Integer = _BoundedInteger
