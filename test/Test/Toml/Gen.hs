{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module contains all generators for @tomland@ testing.

module Test.Toml.Gen
       ( -- * Property
         PropertyTest
       , prop

         -- * Generators
         -- ** Primitive
       , genInt
       , genInteger
       , genDouble
       , genWord
       , genNatural
       , genFloat

       , genList
       , genNonEmpty
       , genHashSet
       , genIntSet

       , genBool

       , genText
       , genString
       , genByteString
       , genLByteString
       , genLText

         -- ** Dates
       , genDay
       , genHours
       , genLocal
       , genZoned

         -- ** @TOML@ specific
       , genVal
       , genKey
       , genPrefixMap
       , genToml

         -- ** Other
       , range100
       ) where

import Control.Applicative (liftA2)
import Control.Monad (forM, replicateM)
import Data.ByteString (ByteString)
import Data.Fixed (Fixed (..))
import Data.Functor.Identity (Identity)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay (..), ZonedTime (..), fromGregorian,
                  minutesToTimeZone)
import GHC.Exts (fromList)
import GHC.Stack (HasCallStack)
import Hedgehog (Gen, GenBase, MonadGen, PropertyT, Range, property)
import Numeric.Natural (Natural)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Toml.Bi.Map (toMArray)
import Toml.PrefixTree (pattern (:||), Key (..), Piece (..), PrefixMap, PrefixTree (..))
import Toml.Type (AnyValue (..), TOML (..), TValue (..), Value (..))

import qualified Data.ByteString.Lazy as LB
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Toml.PrefixTree as Toml (fromList)

----------------------------------------------------------------------------
-- Property test creator
----------------------------------------------------------------------------

type PropertyTest = [TestTree]

prop :: HasCallStack => TestName -> PropertyT IO () -> [TestTree]
prop testName = pure . testProperty testName . property

----------------------------------------------------------------------------
-- Common generators
----------------------------------------------------------------------------

-- @TOML@ specific

type V = Int

genVal :: MonadGen m => m V
genVal = Gen.int (Range.constant 0 256)

-- | Generates random value of 'AnyValue' type.
genAnyValue :: (MonadGen m, GenBase m ~ Identity) => m AnyValue
genAnyValue = Gen.choice $
    (AnyValue <$> genArray) : noneArrayList

-- | Generate either a bare piece, or a quoted piece
genPiece :: forall m . (MonadGen m, GenBase m ~ Identity) => m Piece
genPiece = Piece <$> Gen.choice [bare, quoted]
  where
    alphadashes :: m Char
    alphadashes = Gen.choice [Gen.alphaNum, Gen.element "_-"]

    notControl :: m Char
    notControl = Gen.filter (not . Char.isControl) Gen.unicode

    bare :: m Text
    bare = Gen.text (Range.constant 1 10) alphadashes

    wrapChar :: Char -> Text -> Text
    wrapChar c = Text.cons c . (`Text.append` Text.singleton c)

    quotedWith :: Char -> m Text
    quotedWith c = wrapChar c <$> Gen.text (Range.constant 1 10) (Gen.filter (/= c) notControl)

    quoted :: m Text
    quoted = Gen.filter (not . endsWithEscape) $ Gen.choice [quotedWith '"', quotedWith '\'']

genKey :: (MonadGen m, GenBase m ~ Identity) => m Key
genKey = Key <$> Gen.nonEmpty (Range.constant 1 10) genPiece

genKeyAnyValue :: (MonadGen m, GenBase m ~ Identity) => m (Key, AnyValue)
genKeyAnyValue = liftA2 (,) genKey genAnyValue

genKeyAnyValueList :: (MonadGen m, GenBase m ~ Identity) => m [(Key, AnyValue)]
genKeyAnyValueList = Gen.list (Range.linear 0 10) genKeyAnyValue

-- Generates key-value pair for PrefixMap
genEntry :: (MonadGen m, GenBase m ~ Identity) => m (Piece, Key)
genEntry = genKey >>= \case
    key@(piece :|| _) -> pure (piece, key)

genPrefixMap :: (MonadGen m, GenBase m ~ Identity) => m (PrefixMap V)
genPrefixMap = do
    entries <- Gen.list (Range.linear 0 10) genEntry
    kvps    <- forM entries $ \(piece, key) -> do
        tree <- genPrefixTree key
        pure (piece, tree)

    pure $ fromList kvps

genPrefixTree :: forall m . (MonadGen m, GenBase m ~ Identity) => Key -> m (PrefixTree V)
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

makeToml :: [(Key, AnyValue)] -> TOML
makeToml kv = TOML (fromList kv) mempty mempty

genToml :: (MonadGen m, GenBase m ~ Identity) => m TOML
genToml = Gen.recursive
            Gen.choice
            [ makeToml <$> genKeyAnyValueList ]
            [ TOML <$> keyValues <*> tables <*> arrays ]
  where
    keyValues = fromList <$> genKeyAnyValueList
    tables = fmap Toml.fromList
             $ Gen.list (Range.linear 0 5)
             $ (,) <$> genKey <*> genToml
    arrays = fmap fromList $
             Gen.list (Range.linear 0 10) $ do
               key <- genKey
               arr <- Gen.list (Range.linear 1 10) genToml
               return (key, NE.fromList arr)

-- Date generators

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

-- Primitive generators

range100 :: Range Int
range100 = Range.constant 0 100

genInt :: MonadGen m => m Int
genInt = Gen.int Range.constantBounded

genInteger :: MonadGen m => m Integer
genInteger = toInteger <$> genInt

genDouble :: MonadGen m => m Double
genDouble = Gen.frequency
    [ (10, Gen.double $ Range.constant @Double (-1000000.0) 1000000.0)
    , (1, Gen.constant $ 1/0)
    , (1, Gen.constant $ -1/0)
    , (1, Gen.constant $ 0/0)
    ]

genWord :: MonadGen m => m Word
genWord = Gen.word Range.constantBounded

genNatural :: MonadGen m => m Natural
genNatural = fromIntegral <$> genWord

genFloat :: MonadGen m => m Float
genFloat = Gen.float (Range.constant (-10000.0) 10000.0)

genHashSet :: (Eq a, Hashable a) => Gen a -> Gen (HashSet a)
genHashSet genA = fromList <$> genList genA

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty = Gen.nonEmpty (Range.constant 1 5)

genList :: Gen a -> Gen [a]
genList = Gen.list range100

genIntSet :: Gen IntSet
genIntSet = fromList <$> genList genInt

genBool :: MonadGen m => m Bool
genBool = Gen.bool

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
genText :: (MonadGen m, GenBase m ~ Identity) => m Text
genText = Gen.filter (not . endsWithEscape) $ fmap Text.concat $ Gen.list (Range.constant 0 256) $ Gen.choice
    [ Text.singleton <$> Gen.alphaNum
    , genEscapeSequence
    , genPunctuation
    , genUniHex4Color
    , genUniHex8Color
    ]

genString :: Gen String
genString = Text.unpack <$> genText

genByteString :: Gen ByteString
genByteString = Gen.utf8 range100 Gen.alphaNum

genLByteString :: Gen LB.ByteString
genLByteString = LB.fromStrict <$> genByteString

genLText :: Gen L.Text
genLText = L.fromStrict <$> genText

-- | List of AnyValue generators.
noneArrayList :: (MonadGen m, GenBase m ~ Identity) => [m AnyValue]
noneArrayList =
    [ AnyValue . Bool    <$> genBool
    , AnyValue . Integer <$> genInteger
    , AnyValue . Double  <$> genDouble
    , AnyValue . Text    <$> genText
    , AnyValue . Zoned   <$> genZoned
    , AnyValue . Local   <$> genLocal
    , AnyValue . Day     <$> genDay
    , AnyValue . Hours   <$> genHours
    ]

genArrayFrom :: MonadGen m => m AnyValue -> m (Value 'TArray)
genArrayFrom noneArray = do
    eVal <- toMArray <$> Gen.list (Range.constant 0 5) noneArray
    case eVal of
        Left err  -> error $ show err
        Right val -> pure val

{- | Generate arrays and nested arrays. For example:
Common array:
Array [Double (-563397.0197456297),Double (-308866.62837749254),Double (-29555.32072604308),Double 772371.8575471763,Double (-880016.1210667372),Double 182763.78796234122,Double (-462893.41157520143),Double 814856.6483699235,Double (-454629.17640282493)]
Nested array of AnyValue:
Array [Array [Text "ACyz38VcLz0hxwdFkHTU6PYK8h8CeaiEpI2xAaiZTKBQ3zC1W717cZY35lk8EAK6pPw3WvwIdNktxIV2LrvFSpU8ee6zkXvpvePitW9aspAeeOCF9Q9ry20y7skFZ2qShi7CSx8888zWIqyc8iBkoLNvq4fONLtuUqSw2SlNee4hDIwrnx5O4RuHW1dQfJcnC34h9S0DlIGYP08qq6QHxO4E0HE74cNmiViGm3xpDC8Ro5D8Y6p0FLSN1ELq9Lwm",Text "HhNv0LKICdlKxN"],Array [Integer 986479839551009895,Integer 8636972066308796678,Integer (-3464941350081979804),Integer (-6560688879547055621),Integer (-4749037439349044738)],Array []]
-}
genArray :: (MonadGen m, GenBase m ~ Identity) => m (Value 'TArray)
genArray = Gen.recursive Gen.choice
    [Gen.choice $ map genArrayFrom noneArrayList]
    [Array <$> Gen.list (Range.constant 0 5) genArray]

-- filters

-- | True if Text ends with an escape character
endsWithEscape :: Text -> Bool
endsWithEscape t
    | t == Text.empty = False
    | Text.last t == '\\' = True
    | otherwise = False
