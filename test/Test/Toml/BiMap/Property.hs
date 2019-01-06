{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeApplications #-}

module Test.Toml.BiMap.Property where

import Hedgehog (Gen, PropertyT, Range, assert, forAll, tripping, (===))

import Data.Time (Day, ZonedTime (..))
import GHC.Natural (Natural)
import Test.Tasty (testGroup)
import Test.Toml.Gen (PropertyTest, prop)
import Toml.Bi.Map (BiMap (..), TomlBiMap)

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashSet as HS
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Toml.Gen as G
import qualified Toml.Bi.Map as B

-- Custom generators

range100 :: Range Int
range100 = Range.constant 0 100

genNatural :: Gen Natural
genNatural = fromIntegral <$> Gen.word Range.constantBounded

genInSet :: Gen IS.IntSet
genInSet = IS.fromList <$> Gen.list range100 (Gen.int Range.constantBounded)

genHashSet :: Gen (HS.HashSet Integer)
genHashSet = HS.fromList <$> Gen.list range100 G.genInteger

genNEDays :: Gen (NE.NonEmpty Day)
genNEDays = Gen.nonEmpty (Range.constant 1 100) G.genDay

genLByteString :: Gen BL.ByteString
genLByteString =
  BB.toLazyByteString . BB.byteString <$>
  Gen.utf8 range100 Gen.unicodeAll

-- Tests

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
    , prop "Natural" (testBiMap B._Natural genNatural)
    , prop "Int" (testBiMap B._Int (Gen.int Range.constantBounded))
    , prop "Word" (testBiMap B._Word (Gen.word Range.constantBounded))
    , prop "Double" testDouble
    , prop "Float"
        (testBiMap B._Float (Gen.float $ Range.constant (-10000.0) 10000.0))
    , prop "Text" (testBiMap B._Text G.genText)
    , prop "String"
        (testBiMap B._String (Gen.string range100 Gen.unicode))
    , prop "Read (Integer)" (testBiMap B._Read G.genInteger)
    , prop "ByteString"
        (testBiMap B._ByteString (Gen.utf8 range100 Gen.unicodeAll))
    , prop "Lazy ByteString" (testBiMap B._LByteString genLByteString)
    , prop "ZonedTime" (testBiMap B._ZonedTime G.genZoned)
    , prop "LocalTime" (testBiMap B._LocalTime G.genLocal)
    , prop "TimeOfDay" (testBiMap B._TimeOfDay G.genHours)
    , prop "Day" (testBiMap B._Day G.genDay)
    , prop "IntSet" (testBiMap B._IntSet genInSet)
    , prop "Array (Day)"
        (testBiMap (B._Array B._Day) (Gen.list range100 G.genDay))
    , prop "Set (Day)" (testBiMap (B._Set B._Day) (Gen.set range100 G.genDay))
    , prop "NonEmpty (Day)" (testBiMap (B._NonEmpty B._Day) genNEDays)
    , prop "HashSet (Integer)" (testBiMap (B._HashSet B._Integer) genHashSet)
    ]

-- Orphan instances

instance Eq ZonedTime where
  (ZonedTime a b) == (ZonedTime c d) = a == c && b == d
