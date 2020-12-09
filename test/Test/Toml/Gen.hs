{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module contains all generators for @tomland@ testing.

module Test.Toml.Gen
       ( -- * Generators
         -- ** Primitive
         genBool
       , genInt
       , genInteger
       , genDouble
       , genWord
       , genWord8
       , genNatural
       , genFloat

       , genList
       , genSmallList
       , genNonEmpty
       , genSet
       , genHashSet
       , genIntSet

       , genMap

       , genText
       , genString
       , genByteString
       , genLByteString
       , genLText

         -- ** Dates
       , genDay
       , genTimeOfDay
       , genLocalTime
       , genZonedTime

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
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay (..), ZonedTime (..), fromGregorian,
                  minutesToTimeZone)
import Data.Word (Word8)
import GHC.Exts (fromList)
import Hedgehog (Gen, Range)
import Numeric.Natural (Natural)

import Toml.Type.AnyValue (AnyValue (..), toMArray)
import Toml.Type.Key (pattern (:||), Key (..), Piece (..))
import Toml.Type.PrefixTree (PrefixMap, PrefixTree (..))
import Toml.Type.TOML (TOML (..))
import Toml.Type.Value (TValue (..), Value (..))

import qualified Data.ByteString.Lazy as LB
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Toml.Type.PrefixTree as Toml (fromList)


----------------------------------------------------------------------------
-- Common generators
----------------------------------------------------------------------------

-- @TOML@ specific

type V = Int

genVal :: Gen V
genVal = Gen.int (Range.constant 0 256)

-- | Generates random value of 'AnyValue' type.
genAnyValue :: Gen AnyValue
genAnyValue = Gen.choice $
    (AnyValue <$> genArray) : noneArrayList

-- | Generate either a bare piece, or a quoted piece
genPiece :: Gen Piece
genPiece = Piece <$> Gen.choice [bare, quoted]
  where
    bare :: Gen Text
    bare = liftA2 Text.cons Gen.alpha $ Gen.text (Range.constant 1 10) alphadashes

    alphadashes :: Gen Char
    alphadashes = Gen.choice [Gen.alphaNum, Gen.element "_-"]

    quoted :: Gen Text
    quoted = genNotEscape $ Gen.choice [quotedWith '"', quotedWith '\'']

    quotedWith :: Char -> Gen Text
    quotedWith c = wrapChar c <$> Gen.text (Range.constant 1 10) notControl
      where
        notControl :: Gen Char
        notControl = Gen.filter (\x -> x /= c && not (Char.isControl x)) Gen.unicode

    wrapChar :: Char -> Text -> Text
    wrapChar c = Text.cons c . (`Text.append` Text.singleton c)

genKey :: Gen Key
genKey = Key <$> Gen.nonEmpty (Range.constant 1 10) genPiece

genKeyAnyValue :: Gen (Key, AnyValue)
genKeyAnyValue = liftA2 (,) genKey genAnyValue

genKeyAnyValueList :: Gen [(Key, AnyValue)]
genKeyAnyValueList = Gen.list (Range.linear 0 10) genKeyAnyValue

-- Generates key-value pair for PrefixMap
genEntry :: Gen (Piece, Key)
genEntry = genKey >>= \case
    key@(piece :|| _) -> pure (piece, key)

genPrefixMap :: Gen (PrefixMap V)
genPrefixMap = do
    entries <- Gen.list (Range.linear 0 10) genEntry
    kvps    <- forM entries $ \(piece, key) -> do
        tree <- genPrefixTree key
        pure (piece, tree)

    pure $ fromList kvps

genPrefixTree :: Key -> Gen (PrefixTree V)
genPrefixTree key = Gen.recursive
    -- list picker generator combinator
    Gen.choice
    -- non-recursive generators
    [ Leaf key <$> genVal ]
    -- recursive generators
    [ genPrefixMap >>= genBranch ]
  where
    genBranch :: PrefixMap V -> Gen (PrefixTree V)
    genBranch prefMap = do
        prefVal <- Gen.maybe genVal
        pure $ Branch key prefVal prefMap

makeToml :: [(Key, AnyValue)] -> TOML
makeToml kv = TOML (fromList kv) mempty mempty

genToml :: Gen TOML
genToml = Gen.recursive
    Gen.choice
    [ makeToml <$> genKeyAnyValueList ]
    [ TOML <$> keyValues <*> tables <*> arrays ]
  where
    keyValues :: Gen (HashMap Key AnyValue)
    keyValues = fromList <$> genKeyAnyValueList

    tables :: Gen (PrefixMap TOML)
    tables = fmap Toml.fromList
        $ Gen.list (Range.linear 0 5)
        $ (,) <$> genKey <*> genToml

    arrays :: Gen (HashMap Key (NonEmpty TOML))
    arrays = fmap fromList $ Gen.list (Range.linear 0 10) $ do
        key <- genKey
        arr <- Gen.list (Range.linear 1 10) genToml
        pure (key, NE.fromList arr)

-- Date generators

genDay :: Gen Day
genDay = do
    y <- toInteger <$> Gen.int (Range.constant 1968 2019)
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 28)
    pure $ fromGregorian y m d

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = do
    secs <- MkFixed <$> Gen.integral (Range.constant 0 61)
    mins <- Gen.int (Range.constant 0 59)
    hours <- Gen.int (Range.constant 0 23)
    pure $ TimeOfDay hours mins secs

genLocalTime :: Gen LocalTime
genLocalTime = do
    day <- genDay
    LocalTime day <$> genTimeOfDay

genZonedTime :: Gen ZonedTime
genZonedTime = do
    local <- genLocalTime
    zMin <- Gen.int (Range.constant (-720) 720)
    let zTime = minutesToTimeZone zMin
    pure $ ZonedTime local zTime

-- Primitive generators

range100 :: Range Int
range100 = Range.constant 0 100

genBool :: Gen Bool
genBool = Gen.bool

genInt :: Gen Int
genInt = Gen.int Range.constantBounded

genInteger :: Gen Integer
genInteger = toInteger <$> genInt

genDouble :: Gen Double
genDouble = Gen.frequency
    [ (10, Gen.double $ Range.constant @Double (-1000000.0) 1000000.0)
    , (1, Gen.constant $ 1/0)
    , (1, Gen.constant $ -1/0)
    , (1, Gen.constant $ 0/0)
    ]

genWord :: Gen Word
genWord = Gen.word Range.constantBounded

genWord8 :: Gen Word8
genWord8 = Gen.word8 Range.constantBounded

genNatural :: Gen Natural
genNatural = fromIntegral <$> genWord

genFloat :: Gen Float
genFloat = Gen.float (Range.constant (-10000.0) 10000.0)

genSet :: Ord a => Gen a -> Gen (Set a)
genSet genA = fromList <$> genList genA

genHashSet :: (Eq a, Hashable a) => Gen a -> Gen (HashSet a)
genHashSet genA = fromList <$> genList genA

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty = Gen.nonEmpty (Range.constant 1 5)

genList :: Gen a -> Gen [a]
genList = Gen.list range100

genSmallList :: Gen a -> Gen [a]
genSmallList = Gen.list $ Range.constant 0 10

genIntSet :: Gen IntSet
genIntSet = fromList <$> genList genInt

genMap :: Ord k => Gen k -> Gen v -> Gen (Map k v)
genMap genK genV = Map.fromList <$> genSmallList (liftA2 (,) genK genV)

-- | Generatates control sympol.
genEscapeSequence :: Gen Text
genEscapeSequence = Gen.element
    [ "\n", "\b", "\f", "\r", "\t", "\\", "\"" ]

-- | Generatates punctuation.
genPunctuation :: Gen Text
genPunctuation = Gen.element
    [ ",", ".", ":", ";", "'", "?", "!", "`"
    , "-", "_", "*", "$", "#", "@", "(", ")"
    , " ", "^", "#", "/","&", ">", "<"
    ]

-- | Generatates n length list of hex chars.
genDiffHex :: Int -> Gen String
genDiffHex n = replicateM n Gen.hexit

-- | Generates unicode color string (u1234)
genUniHex4Color :: Gen Text
genUniHex4Color = do
    hex <- genDiffHex 4
    pure . Text.pack $ "\\u" ++ hex

-- | Generates unicode color string (u12345678)
genUniHex8Color :: Gen Text
genUniHex8Color = do
    hex <- genDiffHex 8
    pure . Text.pack $ "\\U" ++ hex

-- | Generates some unescaped unicode string
genUnicodeChar :: Gen Text
genUnicodeChar = Gen.element
    [ "č", "ć", "š", "đ", "ž", "Ö", "ё"
    , "в", "ь", "ж", "ю", "ч", "ü", "я"
    ]

-- | Generates text from different symbols.
genText :: Gen Text
genText = genNotEscape $ fmap Text.concat $ Gen.list (Range.constant 0 256) $ Gen.choice
    [ Text.singleton <$> Gen.alphaNum
    , genEscapeSequence
    , genPunctuation
    , genUniHex4Color
    , genUniHex8Color
    --, genUnicodeChar
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
noneArrayList :: [Gen AnyValue]
noneArrayList =
    [ AnyValue . Bool    <$> genBool
    , AnyValue . Integer <$> genInteger
    , AnyValue . Double  <$> genDouble
    , AnyValue . Text    <$> genText
    , AnyValue . Zoned   <$> genZonedTime
    , AnyValue . Local   <$> genLocalTime
    , AnyValue . Day     <$> genDay
    , AnyValue . Hours   <$> genTimeOfDay
    ]

genArrayFrom :: Gen AnyValue -> Gen (Value 'TArray)
genArrayFrom noneArray = do
    eVal <- toMArray <$> Gen.list (Range.constant 0 5) noneArray
    case eVal of
        Left err  -> error $ show err
        Right val -> pure val

{- | Generate arrays and nested arrays. For example:

Common array:

@
Array
    [ Double (-5.7)
    , Double (-6.4)
    , Double 1.3
    ]
@

Nested array of AnyValue:

@
Array
    [ Array
        [ Text "AH",Text "HA"]
        , Array [Integer 9,Integer (-3)]
        , Array []
        ]
    ]
@
-}
genArray :: Gen (Value 'TArray)
genArray = Gen.recursive Gen.choice
    [Gen.choice $ map genArrayFrom noneArrayList]
    [Array <$> Gen.list (Range.constant 0 5) genArray]

-- filters

-- | Discards strings that end with \
genNotEscape :: Gen Text -> Gen Text
genNotEscape gen = gen >>= \t ->
    if | Text.null t -> pure t
       | Text.last t == '\\' -> Gen.discard
       | otherwise -> pure t

-- Orphan instances

instance Eq ZonedTime where
  (ZonedTime a b) == (ZonedTime c d) = a == c && b == d
