{-# LANGUAGE DataKinds           #-}
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
import Data.Fixed (Fixed (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay (..), ZonedTime (..), fromGregorian,
                  minutesToTimeZone)
import GHC.Stack (HasCallStack)
import Hedgehog (MonadGen, PropertyT, property)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Toml.BiMap (toMArray)
import Toml.PrefixTree (pattern (:||), Key (..), Piece (..), PrefixMap, PrefixTree (..), fromList)
import Toml.Type (AnyValue (..), DateTime (..), TOML (..), TValue (..), Value (..))

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
genAnyValue = Gen.choice
    [ AnyValue . Bool    <$> genBool
    , AnyValue . Integer <$> genInteger
    , AnyValue . Double  <$> genDouble
    , AnyValue . Text    <$> genText
    , AnyValue . Date    <$> genDate
    , AnyValue <$> genArray
    , AnyValue <$> genArrayList
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

genDay :: MonadGen m => m Day
genDay = do
    y <- toInteger <$> Gen.int (Range.constant 1968 2019)
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 28)
    pure $ fromGregorian y m d

genHours :: MonadGen m => m TimeOfDay
genHours = do
    secs <- MkFixed <$> Gen.integral (Range.constant 0 61)
    mins <- Gen.int (Range.constant 0 59)
    hours <- Gen.int (Range.constant 0 23)
    pure $ TimeOfDay hours mins secs

genLocal :: MonadGen m => m LocalTime
genLocal = do
    day <- genDay
    LocalTime day <$> genHours

genZoned :: MonadGen m => m ZonedTime
genZoned = do
    local <- genLocal
    zMin <- Gen.int (Range.constant (-720) 720)
    let zTime = minutesToTimeZone zMin
    pure $ ZonedTime local zTime

genDate :: MonadGen m => m DateTime
genDate = Gen.choice
    [ Day   <$> genDay
    , Hours <$> genHours
    , Local <$> genLocal
    , Zoned <$> genZoned
    ]

genBool :: MonadGen m => m Bool
genBool = Gen.bool

genInteger :: MonadGen m => m Integer
genInteger = toInteger <$> Gen.int (Range.constantBounded @Int)

genDouble :: MonadGen m => m Double
genDouble = Gen.double $ Range.constant @Double (-1000000.0) 1000000.0

genText :: MonadGen m => m Text
genText = Gen.text (Range.constant 0 256) Gen.alphaNum

noneArrayList :: MonadGen m => [m AnyValue]
noneArrayList =
    [ AnyValue . Bool    <$> genBool
    , AnyValue . Integer <$> genInteger
    , AnyValue . Double  <$> genDouble
    , AnyValue . Text    <$> genText
    , AnyValue . Date    <$> genDate
    ]

genArrayFrom :: MonadGen m => m AnyValue -> m (Value 'TArray)
genArrayFrom noneArray = fromMaybe (error "Error in genArrayFrom") . toMArray <$> Gen.list (Range.constant 3 6) noneArray

genArray :: MonadGen m => m (Value 'TArray)
genArray = Gen.recursive Gen.choice
    [Gen.choice $ map genArrayFrom noneArrayList]
    [genArray]

genArrayList :: MonadGen m => m (Value 'TArray)
genArrayList = Array <$> Gen.list (Range.constant 3 6) genArray
