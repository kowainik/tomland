{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Toml.BiMap.Property where

import Hedgehog (Gen, PropertyT, assert, forAll, tripping, (===))

import Data.Time (ZonedTime (..))
import Test.Tasty (testGroup)
import Test.Toml.Gen (PropertyTest, prop)
import Toml.Bi.Map (BiMap (..), TomlBiMap)

import qualified Hedgehog.Gen as Gen
import qualified Test.Toml.Gen as G
import qualified Toml.Bi.Map as B


testBiMap
    :: (Monad m, Show a, Show b, Eq a)
    => TomlBiMap a b
    -> Gen a
    -> PropertyT m ()
testBiMap bimap gen = do
    x <- forAll gen
    tripping x (forward bimap) (backward bimap =<<)

-- Double needs a special test because NaN /= NaN
testDouble :: PropertyT IO ()
testDouble = do
    x <- forAll G.genDouble
    if isNaN x
      then assert $
          fmap isNaN (forward B._Double x >>= backward B._Double) == Right True
      else (forward B._Double x >>= backward B._Double) === Right x

test_BiMaps :: PropertyTest
test_BiMaps = pure $ testGroup "BiMap roundtrip tests" $ concat
    [ prop "Bool" (testBiMap B._Bool G.genBool)
    , prop "Integer" (testBiMap B._Integer G.genInteger)
    , prop "Natural" (testBiMap B._Natural G.genNatural)
    , prop "Int" (testBiMap B._Int G.genInt)
    , prop "Word" (testBiMap B._Word G.genWord)
    , prop "Word8" (testBiMap B._Word8 G.genWord8)
    , prop "Double" testDouble
    , prop "Float" (testBiMap B._Float G.genFloat)
    , prop "Text" (testBiMap B._Text G.genText)
    , prop "LazyText" (testBiMap B._LText G.genLText)
    , prop "String" (testBiMap B._String G.genString)
    , prop "Read (Integer)" (testBiMap B._Read G.genInteger)
    , prop "ByteString" (testBiMap B._ByteString G.genByteString)
    , prop "Lazy ByteString" (testBiMap B._LByteString G.genLByteString)
    , prop "ByteStringArray" (testBiMap B._ByteStringArray G.genByteString)
    , prop "Lazy ByteStringArray" (testBiMap B._LByteStringArray G.genLByteString)
    , prop "ZonedTime" (testBiMap B._ZonedTime G.genZoned)
    , prop "LocalTime" (testBiMap B._LocalTime G.genLocal)
    , prop "TimeOfDay" (testBiMap B._TimeOfDay G.genHours)
    , prop "Day" (testBiMap B._Day G.genDay)
    , prop "IntSet" (testBiMap B._IntSet G.genIntSet)
    , prop "Array (Day)" (testBiMap (B._Array B._Day) (G.genList G.genDay))
    , prop "Set (Day)" (testBiMap (B._Set B._Day) (Gen.set G.range100 G.genDay))
    , prop "NonEmpty (Day)" (testBiMap (B._NonEmpty B._Day) (G.genNonEmpty G.genDay))
    , prop "HashSet (Integer)" (testBiMap (B._HashSet B._Integer) (G.genHashSet G.genInteger))
    ]

-- Orphan instances

instance Eq ZonedTime where
  (ZonedTime a b) == (ZonedTime c d) = a == c && b == d
