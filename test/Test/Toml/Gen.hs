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
import Control.Monad (forM, replicateM)
import Data.Fixed (Fixed (..))
import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay (..), ZonedTime (..), fromGregorian,
                  minutesToTimeZone)
import GHC.Stack (HasCallStack)
import Hedgehog (MonadGen, PropertyT, property)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Toml.BiMap (toMArray, prettyBiMapError)
import Toml.PrefixTree (pattern (:||), Key (..), Piece (..), PrefixMap, PrefixTree (..), fromList)
import Toml.Type (AnyValue (..), DateTime (..), TOML (..), TValue (..), Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
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
genAnyValue = Gen.choice $
    (AnyValue <$> genArray) : noneArrayList

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
genEntry = genKey >>= \case
    key@(piece :|| _) -> pure (piece, key)

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
    makeToml kv = TOML (HashMap.fromList kv) mempty mempty

genToml :: MonadGen m => m TOML
genToml = do
    kv     <- HashMap.fromList <$> genKeyAnyValueList
    tables <- Gen.list (Range.linear 0 10) genTableHeader
    pure $ TOML kv (fromList tables) mempty

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
genDouble = Gen.frequency
    [ (50, Gen.double $ Range.constant @Double (-1000000.0) 1000000.0)
    , (5, Gen.constant $ 1/0)
    , (5, Gen.constant $ -1/0)
    , (5, Gen.constant $ 0/0)
    ]

-- | Generatates control sympol.
genEscapeSequence :: MonadGen m => m Text
genEscapeSequence = Gen.element
    [ "\n", "\b", "\f", "\r", "\t", "\\", "\"" ]

-- | Generatates punctuation.
genPunctuation :: MonadGen m => m Text
genPunctuation = Gen.element
    [ ",", ".", ":", ";", "'", "?", "!", "`"
    , "-", "_", "*", "$", "#", "@", "(", ")"
    , " ", "^", "#", "/","&", ">", "<"
    ]

-- | Generatates n length list of hex chars.
genDiffHex :: MonadGen m => Int -> m String
genDiffHex n = replicateM n Gen.hexit

-- | Generates unicode color string (u1234)
genUniHex4Color :: MonadGen m => m Text
genUniHex4Color = do
    hex <- genDiffHex 4
    pure . Text.pack $ "\\u" ++ hex

-- | Generates unicode color string (u12345678)
genUniHex8Color :: MonadGen m => m Text
genUniHex8Color = do
    hex <- genDiffHex 8
    pure . Text.pack $ "\\U" ++ hex

-- | Generates text from different symbols.
genText :: MonadGen m => m Text
genText =  fmap Text.concat $ Gen.list (Range.constant 0 256) $ Gen.choice
    [ Text.singleton <$> Gen.alphaNum
    , genEscapeSequence
    , genPunctuation
    , genUniHex4Color
    , genUniHex8Color
    ]

-- | List of AnyValue generators.
noneArrayList :: MonadGen m => [m AnyValue]
noneArrayList =
    [ AnyValue . Bool    <$> genBool
    , AnyValue . Integer <$> genInteger
    , AnyValue . Double  <$> genDouble
    , AnyValue . Text    <$> genText
    , AnyValue . Date    <$> genDate
    ]

genArrayFrom :: MonadGen m => m AnyValue -> m (Value 'TArray)
genArrayFrom noneArray = do
    eVal <- toMArray <$> Gen.list (Range.constant 0 5) noneArray
    case eVal of
        Left err -> error $ Text.unpack $ prettyBiMapError err
        Right val -> pure val

{- | Generate arrays and nested arrays. For example:
Common array:
Array [Double (-563397.0197456297),Double (-308866.62837749254),Double (-29555.32072604308),Double 772371.8575471763,Double (-880016.1210667372),Double 182763.78796234122,Double (-462893.41157520143),Double 814856.6483699235,Double (-454629.17640282493)]
Nested array of AnyValue:
Array [Array [Text "ACyz38VcLz0hxwdFkHTU6PYK8h8CeaiEpI2xAaiZTKBQ3zC1W717cZY35lk8EAK6pPw3WvwIdNktxIV2LrvFSpU8ee6zkXvpvePitW9aspAeeOCF9Q9ry20y7skFZ2qShi7CSx8888zWIqyc8iBkoLNvq4fONLtuUqSw2SlNee4hDIwrnx5O4RuHW1dQfJcnC34h9S0DlIGYP08qq6QHxO4E0HE74cNmiViGm3xpDC8Ro5D8Y6p0FLSN1ELq9Lwm",Text "HhNv0LKICdlKxN"],Array [Integer 986479839551009895,Integer 8636972066308796678,Integer (-3464941350081979804),Integer (-6560688879547055621),Integer (-4749037439349044738)],Array []]
-}
genArray :: MonadGen m => m (Value 'TArray)
genArray = Gen.recursive Gen.choice
    [Gen.choice $ map genArrayFrom noneArrayList]
    [Array <$> Gen.list (Range.constant 0 5) genArray]
