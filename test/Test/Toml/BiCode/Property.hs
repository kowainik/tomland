module Test.Toml.BiCode.Property where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Data.ByteString (ByteString)
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

import Test.Toml.Gen (PropertyTest, genBool, genByteString, genDay, genDouble, genFloat, genHashSet,
                      genHours, genInt, genIntSet, genInteger, genLByteString, genLocal, genNatural,
                      genNonEmpty, genString, genText, genWord, genZoned, prop)

import qualified Data.ByteString.Lazy as L
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
    , btDouble        :: Batman Double
    , btFloat         :: Batman Float
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
    , btArrayHashSet  :: HashSet Natural
    , btArrayNonEmpty :: NonEmpty Text
    , btNonEmpty      :: NonEmpty ByteString
    , btList          :: [Bool]
    , btNewtype       :: BigTypeNewtype
    , btSum           :: BigTypeSum
    , btRecord        :: BigTypeRecord
    } deriving (Show, Eq)

-- | Wrapper over 'Double' and 'Float' to be equal on @NaN@ values.
newtype Batman a = Batman a
    deriving (Show)

instance RealFloat a => Eq (Batman a) where
    Batman a == Batman b =
        if isNaN a
            then isNaN b
            else a == b

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
    <*> Toml.diwrap (Toml.double "double")                         .= btDouble
    <*> Toml.diwrap (Toml.float "float")                           .= btFloat
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
    <*> Toml.arrayHashSetOf Toml._Natural    "arrayHashSetDouble"  .= btArrayHashSet
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
        Toml.match (_BigTypeSumA >>> Toml._Integer) "sum.integer"
    <|> Toml.match (_BigTypeSumB >>> Toml._Text)    "sum.text"

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
    btNatural       <- genNatural
    btInt           <- genInt
    btWord          <- genWord
    btDouble        <- Batman <$> genDouble
    btFloat         <- Batman <$> genFloat
    btText          <- genText
    btString        <- genString
    btBS            <- genByteString
    btLazyBS        <- genLByteString
    btLocalTime     <- genLocal
    btDay           <- genDay
    btTimeOfDay     <- genHours
    btArray         <- Gen.list (Range.constant 0 5) genInt
    btArraySet      <- Gen.set (Range.constant 0 5) genWord
    btArrayIntSet   <- genIntSet
    btArrayHashSet  <- genHashSet genNatural
    btArrayNonEmpty <- genNonEmpty genText
    btNonEmpty      <- genNonEmpty genByteString
    btList          <- Gen.list (Range.constant 0 5) genBool
    btNewtype       <- genNewType
    btSum           <- genSum
    btRecord        <- genRec
    pure BigType {..}

-- Custom generators

genNewType :: Gen BigTypeNewtype
genNewType = BigTypeNewtype <$> genZoned

genSum :: Gen BigTypeSum
genSum = Gen.choice
    [ BigTypeSumA <$> genInteger
    , BigTypeSumB <$> genText
    ]

genRec :: Gen BigTypeRecord
genRec = do
    btrBoolSet <- fromList <$> Gen.list (Range.constant 0 5) genBool
    btrNewtypeList <- Gen.list (Range.constant 0 5) genSum
    pure BigTypeRecord{..}
