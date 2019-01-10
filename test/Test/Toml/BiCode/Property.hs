module Test.Toml.BiCode.Property where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime, zonedTimeToUTC)
import GHC.Exts (fromList)
import Hedgehog (Gen, forAll, tripping)
import Numeric.Natural (Natural)

import Toml (TomlBiMap, TomlCodec, (.=))
import Toml.Bi.Code (decode, encode)

import Test.Toml.Gen (PropertyTest, genBool, genDay, genDouble, genHours, genInteger, genLocal,
                      genText, genZoned, prop)

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Toml

test_encodeDecodeProp :: PropertyTest
test_encodeDecodeProp = prop "decode . encode == id" $ do
    bigType <- forAll genBigType
    tripping bigType (encode bigTypeCodec) (decode bigTypeCodec)

data BigType = BigType
    { btBool          :: Bool
    , btInteger       :: Integer
    , btNatural       :: Natural
    , btInt           :: Int
    , btWord          :: Word
    , btDouble        :: Double
    , btFloat         :: Float
    , btText          :: Text
    , btString        :: String
    , btBS            :: ByteString
    , btLazyBS        :: L.ByteString
    , btLocalTime     :: LocalTime
    , btDay           :: Day
    , btTimeOfDay     :: TimeOfDay
    , btArray         :: [Int]
    , btArraySet      :: Set Word
    , btArrayIntSet   :: IntSet
    , btArrayHashSet  :: HashSet Double
    , btArrayNonEmpty :: NonEmpty Text
    , btNonEmpty      :: NonEmpty ByteString
    , btList          :: [Bool]
    , btNewtype       :: BigTypeNewtype
    , btSum           :: BigTypeSum
    , btRecord        :: BigTypeRecord
    } deriving (Show, Eq)

newtype BigTypeNewtype = BigTypeNewtype ZonedTime
    deriving (Show)

instance Eq BigTypeNewtype where
    (BigTypeNewtype a) == (BigTypeNewtype b) = zonedTimeToUTC a == zonedTimeToUTC b

data BigTypeSum = BigTypeSumA Integer | BigTypeSumB Text
    deriving (Show, Eq)

data BigTypeRecord = BigTypeRecord
    { btrBoolSet     :: Set Bool
    , btrNewtypeList :: [BigTypeSum]
    } deriving (Show, Eq)

bigTypeCodec :: TomlCodec BigType
bigTypeCodec = BigType
    <$> Toml.bool                            "bool"                .= btBool
    <*> Toml.integer                         "integer"             .= btInteger
    <*> Toml.natural                         "natural"             .= btNatural
    <*> Toml.int                             "int"                 .= btInt
    <*> Toml.word                            "word"                .= btWord
    <*> Toml.double                          "double"              .= btDouble
    <*> Toml.float                           "float"               .= btFloat
    <*> Toml.text                            "text"                .= btText
    <*> Toml.string                          "string"              .= btString
    <*> Toml.byteString                      "bs"                  .= btBS
    <*> Toml.lazyByteString                  "lbs"                 .= btLazyBS
    <*> Toml.localTime                       "localTime"           .= btLocalTime
    <*> Toml.day                             "day"                 .= btDay
    <*> Toml.timeOfDay                       "timeOfDay"           .= btTimeOfDay
    <*> Toml.arrayOf Toml._Int               "arrayOfInt"          .= btArray
    <*> Toml.arraySetOf Toml._Word           "arraySetOfWord"      .= btArraySet
    <*> Toml.arrayIntSet                     "arrayIntSet"         .= btArrayIntSet
    <*> Toml.arrayHashSetOf Toml._Double     "arrayHashSetDouble"  .= btArrayHashSet
    <*> Toml.arrayNonEmptyOf Toml._Text      "arrayNonEmptyOfText" .= btArrayNonEmpty
    <*> Toml.nonEmpty (Toml.byteString "bs") "nonEmptyBS"          .= btNonEmpty
    <*> Toml.list (Toml.bool "bool")         "listBool"            .= btList
    <*> Toml.diwrap (Toml.zonedTime "nt.zonedTime")                .= btNewtype
    <*> bigTypeSumCodec                                            .= btSum
    <*> Toml.table bigTypeRecordCodec        "table-record"        .= btRecord

_BigTypeSumA :: TomlBiMap BigTypeSum Integer
_BigTypeSumA = Toml.prism BigTypeSumA $ \case
    BigTypeSumA i -> Right i
    other   -> Toml.wrongConstructor "BigTypeSumA" other

_BigTypeSumB :: TomlBiMap BigTypeSum Text
_BigTypeSumB = Toml.prism BigTypeSumB $ \case
    BigTypeSumB n -> Right n
    other    -> Toml.wrongConstructor "BigTypeSumB" other

bigTypeSumCodec :: TomlCodec BigTypeSum
bigTypeSumCodec =
        Toml.match (_BigTypeSumA >>> Toml._Integer) "integer"
    <|> Toml.match (_BigTypeSumB >>> Toml._Text)    "text"

bigTypeRecordCodec :: TomlCodec BigTypeRecord
bigTypeRecordCodec = BigTypeRecord
    <$> Toml.arraySetOf Toml._Bool "rboolSet" .= btrBoolSet
    <*> Toml.list bigTypeSumCodec "rnewtype" .= btrNewtypeList

----------------------------------------------------------------------------
-- Generator
----------------------------------------------------------------------------

genBigType :: Gen BigType
genBigType = do
    btBool          <- genBool
    btInteger       <- genInteger
    btNatural       <- fromInteger <$> genInteger
    btInt           <- genInt
    btWord          <- genWord
    btDouble        <- genDouble
    btFloat         <- genFloat
    btText          <- genText
    btString        <- genString
    btBS            <- genBS
    btLazyBS        <- L.fromStrict <$> genBS
    btLocalTime     <- genLocal
    btDay           <- genDay
    btTimeOfDay     <- genHours
    btArray         <- Gen.list (Range.constant 0 256) genInt
    btArraySet      <- Gen.set (Range.constant 0 256) genWord
    btArrayIntSet   <- genIntSet
    btArrayHashSet  <- genHashSet genDouble
    btArrayNonEmpty <- genNonEmpty genText
    btNonEmpty      <- genNonEmpty genBS
    btList          <- Gen.list (Range.constant 0 256) genBool
    btNewtype       <- genNewType
    btSum           <- genSum
    btRecord        <- genRec
    pure BigType {..}

genInt :: Gen Int
genInt = Gen.int (Range.constant 0 256)

genWord :: Gen Word
genWord = Gen.word (Range.constant 1 256)

genFloat :: Gen Float
genFloat = Gen.float (Range.constant 0 256)

genString :: Gen String
genString = T.unpack <$> genText

genBS :: Gen ByteString
genBS = Gen.bytes (Range.constant 0 256)

genIntSet :: Gen IntSet
genIntSet = fromList <$> Gen.list (Range.constant 0 256) genInt

genHashSet :: (Eq a, Hashable a) => Gen a -> Gen (HashSet a)
genHashSet genA = fromList <$> Gen.list (Range.constant 0 256) genA

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty = Gen.nonEmpty (Range.constant 1 256)

genNewType :: Gen BigTypeNewtype
genNewType = BigTypeNewtype <$> genZoned

genSum :: Gen BigTypeSum
genSum = Gen.choice
    [ BigTypeSumA <$> genInteger
    , BigTypeSumB <$> genText
    ]

genRec :: Gen BigTypeRecord
genRec = do
    btrBoolSet <- Gen.set (Range.constant 0 256) genBool
    btrNewtypeList <- Gen.list (Range.constant 0 256) genSum
    pure BigTypeRecord{..}
