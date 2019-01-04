{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Test.Toml.BiMap.Property where

import Hedgehog (Gen, PropertyT, assert, forAll, tripping, (===))

import Data.Semigroup ((<>))
import Data.Time (ZonedTime (..))
import Test.Toml.Gen (PropertyTest, prop)
import Toml.Bi.Map (BiMap (..), TomlBiMap)

import qualified Data.HashSet as HS
import qualified Data.IntSet as IS
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Toml.Gen as G
import qualified Toml.Bi.Map as B

instance Eq ZonedTime where
  (ZonedTime a b) == (ZonedTime c d) = a == c && b == d

testBiMap :: (Monad m, Show a, Show b, Eq a)
            => TomlBiMap a b
            -> Gen a
            -> PropertyT m ()
testBiMap bimap gen = do
    x <- forAll gen
    tripping x (forward bimap) (backward bimap =<<)

-- Needs a special test because NaN /= NaN
test_Double :: PropertyTest
test_Double =  prop "Double" $ do
    x   <- forAll G.genDouble
    if isNaN x
      then assert $
          fmap isNaN (forward B._Double x >>= backward B._Double) == Right True
      else (forward B._Double x >>= backward B._Double) === Right x

test_BiMaps :: PropertyTest
test_BiMaps =
    prop "Bool" (testBiMap B._Bool G.genBool)
    <> prop "Integer" (testBiMap B._Integer G.genInteger)
    <> prop "Text" (testBiMap B._Text G.genText)
    <> prop "ZonedTime" (testBiMap B._ZonedTime G.genZoned)
    <> prop "LocalTime" (testBiMap B._LocalTime G.genLocal)
    <> prop "TimeOfDay" (testBiMap B._TimeOfDay G.genHours)
    <> prop "Day" (testBiMap B._Day G.genDay)
    <> prop "Word" (testBiMap B._Word (Gen.word Range.constantBounded))
    <> prop "Int" (testBiMap B._Int (Gen.int Range.constantBounded))
    <> prop "Float"
        (testBiMap B._Float (Gen.float $ Range.constant (-1000000.0) 1000000.0))
    <> prop "String"
        (testBiMap B._String (Gen.string (Range.constant 0 100) Gen.unicode))
    <> prop "Read (Integer)" (testBiMap B._Read G.genInteger )
    <> prop "Natural"
        (testBiMap B._Natural (fromIntegral <$> Gen.integral
                             (Range.constant @Integer 0 (2^(65 :: Integer)))))
    <> prop "ByteString"
        (testBiMap B._ByteString (Gen.utf8 (Range.constant 0 100) Gen.unicodeAll))
    <> prop "IntSet"
        (testBiMap B._IntSet (IS.fromList <$> Gen.list (Range.constant 0 100)
                                (Gen.int Range.constantBounded)))
    <> prop "Array (Day)"
        (testBiMap (B._Array B._Day) (Gen.list (Range.constant 0 100) G.genDay))
    <> prop "Set (Day)"
        (testBiMap (B._Set B._Day) (Gen.set (Range.constant 0 100) G.genDay))
    <> prop "NonEmpty (Day)"
        (testBiMap (B._NonEmpty B._Day)
            (Gen.nonEmpty (Range.constant 1 100) G.genDay))
    <> prop "HashSet (Integer)"
        (testBiMap (B._HashSet B._Integer) (HS.fromList <$> Gen.list
                                       (Range.constant 0 100) G.genInteger))


-- TODO _LByteString
