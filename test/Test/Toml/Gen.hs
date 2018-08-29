{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module contains all generators for @tomland@ testing.

module Test.Toml.Gen
       ( -- * Property
         PropertyTest
       , prop

         -- * Generators
       , genVal
       , genKey
       , genPrefixMap
       , genToml
       ) where

import Control.Applicative (liftA2)
import Control.Monad (forM)
import GHC.Stack (HasCallStack)
import Hedgehog (MonadGen, PropertyT, property)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Data.Time (LocalTime(..), TimeOfDay (..), ZonedTime(..), fromGregorian,  TimeZone(..))
import Data.Fixed (Fixed(..))

import Toml.PrefixTree (pattern (:||), Key (..), Piece (..), PrefixMap, PrefixTree (..), fromList)
import Toml.Type (AnyValue (..), TOML (..), Value (..), DateTime (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

----------------------------------------------------------------------------
-- Property test creator
----------------------------------------------------------------------------

type PropertyTest = [TestTree]

prop :: HasCallStack => TestName -> PropertyT IO () -> [TestTree]
prop testName = pure . testProperty testName . property

----------------------------------------------------------------------------
-- Common generators
----------------------------------------------------------------------------

type V = Int

genVal :: MonadGen m => m V
genVal = Gen.int (Range.constant 0 256)

-- TODO: Arrays and Date.
-- | Generates random value of 'AnyValue' type.
genAnyValue :: MonadGen m => m AnyValue
genAnyValue = do
  let randB = Gen.bool
  let randI = toInteger <$> Gen.int (Range.constantBounded @Int)
  let randD = Gen.double $ Range.constant @Double (-1000000.0) 1000000.0
  let randT = Gen.text (Range.constant 0 256) Gen.alphaNum
  let randDay = genDay
  let randH = genHours
  let randZ = genZoned
  let randL = genLocal 
  Gen.choice
      [ AnyValue . Bool    <$> randB
      , AnyValue . Integer <$> randI
      , AnyValue . Double  <$> randD
      , AnyValue . Text    <$> randT
      , AnyValue . Date    <$> randZ 
      , AnyValue . Date    <$> randL
      , AnyValue . Date    <$> randDay
      , AnyValue . Date    <$> randH
      ]

-- TODO: unicode support
genPiece :: MonadGen m => m Piece
genPiece = Piece <$> Gen.text (Range.constant 1 50) Gen.alphaNum

genKey :: MonadGen m => m Key
genKey = Key <$> Gen.nonEmpty (Range.constant 1 10) genPiece

genKeyAnyValue :: MonadGen m => m (Key, AnyValue)
genKeyAnyValue = liftA2 (,) genKey genAnyValue

genKeyAnyValueList :: MonadGen m => m [(Key, AnyValue)]
genKeyAnyValueList = Gen.list (Range.linear 0 10) genKeyAnyValue

-- Generates key-value pair for PrefixMap
genEntry :: MonadGen m => m (Piece, Key)
genEntry = do
    key@(piece :|| _) <- genKey
    pure (piece, key)

genPrefixMap :: MonadGen m => m (PrefixMap V)
genPrefixMap = do
    entries <- Gen.list (Range.linear 0 10) genEntry
    kvps    <- forM entries $ \(piece, key) -> do
        tree <- genPrefixTree key
        pure (piece, tree)

    pure $ HashMap.fromList kvps

genPrefixTree :: forall m . MonadGen m => Key -> m (PrefixTree V)
genPrefixTree key = Gen.recursive
    -- list picker generator combinator
    Gen.choice
    -- non-recursive generators
    [ Leaf key <$> genVal ]
    -- recursive generators
    [ genPrefixMap >>= genBranch ]
  where
    genBranch :: PrefixMap V -> m (PrefixTree V)
    genBranch prefMap = do
        prefVal <- Gen.maybe genVal
        pure $ Branch key prefVal prefMap

genTableHeader :: MonadGen m => m (Key, TOML)
genTableHeader = do
    k <- genKey
    toml <- makeToml <$> genKeyAnyValueList
    pure (k, toml)
  where
    makeToml :: [(Key, AnyValue)] -> TOML
    makeToml kv = TOML (HashMap.fromList kv) mempty

genToml :: MonadGen m => m TOML
genToml = do
    kv     <- HashMap.fromList <$> genKeyAnyValueList
    tables <- Gen.list (Range.linear 0 10) genTableHeader
    pure $ TOML kv (fromList tables)

genDay :: MonadGen m => m DateTime
genDay = do
    y <- toInteger <$> Gen.int (Range.constant 2000 2019)
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 28)
    let day = fromGregorian y m d
    pure $ Day day

genHours :: MonadGen m => m DateTime
genHours = do
    secs <- MkFixed <$> Gen.integral (Range.constant 0 61)
    mins <- Gen.int (Range.constant 0 59)
    hours <- Gen.int (Range.constant 0 23)
    pure $ Hours $ TimeOfDay hours mins secs

genLocal :: MonadGen m => m DateTime
genLocal = do
    Day day <- genDay
    Hours time <- genHours
    let local = LocalTime day time
    pure $ Local local

genZoned :: MonadGen m => m DateTime
genZoned = do
    Local local <- genLocal
    zMin <- Gen.int (Range.constant 0 59)
    zSummer <- Gen.bool
    zName <- Gen.string (Range.constant 0 4) Gen.alphaNum
    let zTime = TimeZone zMin zSummer zName
    let zoned = ZonedTime local zTime
    pure $ Zoned zoned
