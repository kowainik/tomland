{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Toml.Codec.BiMap.Conversion
    ( conversionSpec
    ) where

import Data.Time (ZonedTime (..))
import Hedgehog (Gen, PropertyT, assert, forAll, tripping, (===))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Toml.Codec.BiMap (BiMap (..), TomlBiMap)

import qualified Hedgehog.Gen as Gen

import qualified Test.Toml.Gen as G
import qualified Toml.Codec.BiMap.Conversion as B


conversionSpec :: Spec
conversionSpec = describe "BiMap Rountrip Property tests" $ do
    describe "Primitive" $ do
        it "Bool"            $ testBiMap B._Bool G.genBool
        it "Int"             $ testBiMap B._Int G.genInt
        it "Word"            $ testBiMap B._Word G.genWord
        it "Word8"           $ testBiMap B._Word8 G.genWord8
        it "Integer"         $ testBiMap B._Integer G.genInteger
        it "Natural"         $ testBiMap B._Natural G.genNatural
        it "Double"          testDouble
        it "Float"           $ testBiMap B._Float G.genFloat
        it "Text"            $ testBiMap B._Text G.genText
        it "LazyText"        $ testBiMap B._LText G.genLText
        it "ByteString"      $ testBiMap B._ByteString G.genByteString
        it "Lazy ByteString" $ testBiMap B._LByteString G.genLByteString
        it "String"          $ testBiMap B._String G.genString

    describe "Time" $ do
        it "ZonedTime" $ testBiMap B._ZonedTime G.genZoned
        it "LocalTime" $ testBiMap B._LocalTime G.genLocal
        it "Day"       $ testBiMap B._Day G.genDay
        it "TimeOfDay" $ testBiMap B._TimeOfDay G.genHours

    describe "Arrays" $ do
        it "Array (Int)"      $ testBiMap (B._Array B._Int) (G.genList G.genInt)
        it "Array (Day)"      $ testBiMap (B._Array B._Day) (G.genList G.genDay)
        it "NonEmpty (Int)"   $ testBiMap (B._NonEmpty B._Int) (G.genNonEmpty G.genInt)
        it "Set (Int)"        $ testBiMap (B._Set B._Int) (G.genSet G.genInt)
        it "HashSet (Int)"    $ testBiMap (B._HashSet B._Int) (G.genHashSet G.genInt)
        it "IntSet"           $ testBiMap B._IntSet G.genIntSet
        it "ByteStringArray"  $ testBiMap B._ByteStringArray G.genByteString
        it "LByteStringArray" $ testBiMap B._LByteStringArray G.genLByteString

    describe "Custom" $ do
        it "EnumBounded (Ordering)" $ testBiMap B._EnumBounded $ Gen.enumBounded @_ @Ordering
        it "Read (Integer)"         $ testBiMap B._Read G.genInteger
        it "TextBy (Text)"          $ testBiMap (B._TextBy id Right) G.genText

    describe "Key" $ do
        it "KeyText"   $ testBiMap B._KeyText G.genKey
        it "KeyString" $ testBiMap B._KeyString G.genKey

testBiMap
    :: (Show a, Show b, Eq a)
    => TomlBiMap a b
    -> Gen a
    -> PropertyT IO ()
testBiMap bimap gen = hedgehog $ do
    x <- forAll gen
    tripping x (forward bimap) (backward bimap =<<)

-- Double needs a special test because NaN /= NaN
testDouble :: PropertyT IO ()
testDouble = hedgehog $ do
    x <- forAll G.genDouble
    if isNaN x
    then assert $
        fmap isNaN (forward B._Double x >>= backward B._Double) == Right True
    else (forward B._Double x >>= backward B._Double) === Right x

-- Orphan instances

instance Eq ZonedTime where
  (ZonedTime a b) == (ZonedTime c d) = a == c && b == d
