{-# LANGUAGE PatternSynonyms #-}

module Test.Toml.Codec.BiMap.Conversion
    ( conversionSpec
    ) where

import Hedgehog (Gen, PropertyT, forAll, tripping)
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.Codec.Combinator.Common (Batman (..), _BatmanDouble)
import Toml.Codec.BiMap (BiMap (..), TomlBiMap, tShow)
import Toml.Type.Key (pattern (:||), Piece (..))

import qualified Hedgehog.Gen as Gen

import qualified Test.Toml.Gen as G
import qualified Toml.Codec.BiMap.Conversion as B


conversionSpec :: Spec
conversionSpec = parallel $ describe "BiMap Rountrip Property tests" $ do
    describe "Primitive" $ do
        it "Bool"            $ testBiMap B._Bool G.genBool
        it "Int"             $ testBiMap B._Int G.genInt
        it "Word"            $ testBiMap B._Word G.genWord
        it "Word8"           $ testBiMap B._Word8 G.genWord8
        it "Integer"         $ testBiMap B._Integer G.genInteger
        it "Natural"         $ testBiMap B._Natural G.genNatural
        it "Double"          $ testBiMap _BatmanDouble (Batman <$> G.genDouble)
        it "Float"           $ testBiMap B._Float G.genFloat
        it "Text"            $ testBiMap B._Text G.genText
        it "LazyText"        $ testBiMap B._LText G.genLText
        it "ByteString"      $ testBiMap B._ByteString G.genByteString
        it "Lazy ByteString" $ testBiMap B._LByteString G.genLByteString
        it "String"          $ testBiMap B._String G.genString

    describe "Time" $ do
        it "ZonedTime" $ testBiMap B._ZonedTime G.genZonedTime
        it "LocalTime" $ testBiMap B._LocalTime G.genLocalTime
        it "Day"       $ testBiMap B._Day G.genDay
        it "TimeOfDay" $ testBiMap B._TimeOfDay G.genTimeOfDay

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
        it "Hardcoded (Text)"       $ do
            txt <- forAll G.genText
            testBiMap (B._Hardcoded txt) (pure txt)

    describe "Key" $ do
        it "KeyText"   $ testBiMap B._KeyText G.genKey
        it "KeyString" $ testBiMap B._KeyString G.genKey
        it "KeyInt"    $ testBiMap B._KeyInt ((:|| []) . Piece . tShow <$> G.genInt)

testBiMap
    :: (Show a, Show b, Eq a)
    => TomlBiMap a b
    -> Gen a
    -> PropertyT IO ()
testBiMap bimap gen = hedgehog $ do
    x <- forAll gen
    tripping x (forward bimap) (backward bimap =<<)
