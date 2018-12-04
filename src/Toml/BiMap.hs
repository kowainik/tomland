{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}

{- | Implementation of partial bidirectional mapping as a data type.
-}

module Toml.BiMap
       ( -- * BiMap idea
         BiMap (..)
       , TomlBiMapError (..)
       , TomlBiMap
       , invert
       , iso
       , prism

         -- * Helpers for BiMap and AnyValue
       , mkAnyValueBiMap
       , _TextBy
       , _NaturalInteger
       , _StringText
       , _ReadString
       , _BoundedInteger
       , _ByteStringText
       , _LByteStringText

         -- * Some predefined bi mappings
       , _Array
       , _Bool
       , _Double
       , _Integer
       , _Text
       , _ZonedTime
       , _LocalTime
       , _Day
       , _TimeOfDay
       , _String
       , _Read
       , _Natural
       , _Word
       , _Int
       , _Float
       , _ByteString
       , _LByteString
       , _Set
       , _IntSet
       , _HashSet
       , _NonEmpty

       , _Left
       , _Right
       , _Just

         -- * Useful utility functions
       , toMArray
       , prettyBiMapError
       ) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Word (Word)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.Read (readEither)

import Toml.Type (AnyValue (..), DateTime (..), MatchError (..), TValue, Value (..), leftMatchError,
                  matchArray, matchBool, matchDate, matchDouble, matchInteger, matchText, tShow,
                  toMArray, typeName)

import qualified Control.Category as Cat
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashSet as HS
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


----------------------------------------------------------------------------
-- BiMap concepts and ideas
----------------------------------------------------------------------------

{- | Partial bidirectional isomorphism. @BiMap a b@ contains two function:

1. @a -> Either e b@
2. @b -> Either e a@
-}
data BiMap e a b = BiMap
    { forward  :: a -> Either e b
    , backward :: b -> Either e a
    }

instance Cat.Category (BiMap e) where
    id :: BiMap e a a
    id = BiMap Right Right

    (.) :: BiMap e b c -> BiMap e a b -> BiMap e a c
    bc . ab = BiMap
        { forward  =  forward ab >=>  forward bc
        , backward = backward bc >=> backward ab
        }

-- | Inverts bidirectional mapping.
invert :: BiMap e a b -> BiMap e b a
invert (BiMap f g) = BiMap g f

-- | Creates 'BiMap' from isomorphism.
iso :: (a -> b) -> (b -> a) -> BiMap e a b
iso f g = BiMap (Right . f) (Right . g)

-- | Creates 'BiMap' from prism-like pair of functions.
prism :: (field -> object) -> (object -> Either error field) -> BiMap error object field
prism review preview = BiMap preview (Right . review)

----------------------------------------------------------------------------
-- BiMap error types
----------------------------------------------------------------------------

-- | Abstraction on BiMap with error.
type TomlBiMap = BiMap TomlBiMapError

-- | BiMap error type.
data TomlBiMapError
    = WrongConstructor      -- ^ Error for cases with wrong constructors. For example, you're trying to convert 'Left' but bidirectional converter expects 'Right'
        Text                -- ^ Expected constructor name
        Text                -- ^ Actual Value; TODO: use Show here?
    | WrongValue MatchError -- ^ Error for cases with wrong values.
    | ArbitraryError Text
    deriving (Eq, Show, Generic, NFData)

-- | Converts 'TomlBiMapError' into pretty human-readable text.
prettyBiMapError :: TomlBiMapError -> Text
prettyBiMapError = \case
    WrongConstructor expected actual -> T.unlines
        [ "Invalid constructor"
        , "  * Expected: " <> expected
        , "  * Actual:   " <> actual
        ]
    WrongValue (MatchError expect actual) -> T.unlines
        [ "Invalid value constructor. Expected "
        , tShow expect
        , " Actual value: "
        , tShow actual
        ]
    ArbitraryError text  -> "Invalid value: " <> text

-- | Helper to construct WrongConstuctor error.
wrongConstructor :: Show a => Text -> a -> Either TomlBiMapError b
wrongConstructor constructor x = Left $ WrongConstructor constructor (tShow x)

-- | Left error part of TomlBiMapError.
leftWrongValue :: forall (t :: TValue) b . Value t -> Either TomlBiMapError b
leftWrongValue = Left . WrongValue . MatchError (typeName @TValue) . AnyValue

-- | Error convertor
toTomlBiMapError :: Either MatchError b -> Either TomlBiMapError b
toTomlBiMapError = either (Left . WrongValue) Right

-- | Error convertor. Only for WrongValue constructor.
toMatchError :: Either TomlBiMapError b -> Either MatchError b
toMatchError = either toMatchError' Right
  where
    toMatchError' = \case
        WrongValue matchError -> Left matchError
        _ -> error "Not WrongValue error!"

----------------------------------------------------------------------------
-- General purpose bimaps
----------------------------------------------------------------------------

-- | Bimap for 'Either' and its left type
_Left :: (Show l, Show r) => TomlBiMap (Either l r) l
_Left = prism Left $ \case
    Left l -> Right l
    x -> wrongConstructor "Left" x

-- | Bimap for 'Either' and its right type
_Right :: (Show l, Show r) => TomlBiMap (Either l r) r
_Right = prism Right $ \case
    Right r -> Right r
    x -> wrongConstructor "Right" x

-- | Bimap for 'Maybe'
_Just :: Show r => TomlBiMap (Maybe r) r
_Just = prism Just $ \case
    Just r -> Right r
    x -> wrongConstructor "Just" x

----------------------------------------------------------------------------
--  BiMaps for value
----------------------------------------------------------------------------

-- | Creates prism for 'AnyValue'.
mkAnyValueBiMap
    :: (forall t . Value t -> Either MatchError a)
    -> (a -> Value tag)
    -> TomlBiMap a AnyValue
mkAnyValueBiMap matchValue toValue = BiMap (Right . AnyValue . toValue)
    (\(AnyValue value) -> either (\_-> leftWrongValue value) Right $ matchValue value)

-- | Creates bimap for 'Text' to 'AnyValue' with custom functions
_TextBy :: (a -> Text) -> (Text -> Either MatchError a) -> TomlBiMap a AnyValue
_TextBy toText parseText = mkAnyValueBiMap (matchText >=> parseText) (Text . toText)

-- | 'Bool' bimap for 'AnyValue'. Usually used with 'bool' combinator.
_Bool :: TomlBiMap Bool AnyValue
_Bool = mkAnyValueBiMap matchBool Bool

-- | 'Integer' bimap for 'AnyValue'. Usually used with 'integer' combinator.
_Integer :: TomlBiMap Integer AnyValue
_Integer = mkAnyValueBiMap matchInteger Integer

-- | 'Double' bimap for 'AnyValue'. Usually used with 'double' combinator.
_Double :: TomlBiMap Double AnyValue
_Double = mkAnyValueBiMap matchDouble Double

-- | 'Text' bimap for 'AnyValue'. Usually used with 'text' combinator.
_Text :: TomlBiMap Text AnyValue
_Text = mkAnyValueBiMap matchText Text

-- | Zoned time bimap for 'AnyValue'. Usually used with 'zonedTime' combinator.
_ZonedTime :: TomlBiMap ZonedTime AnyValue
_ZonedTime = mkAnyValueBiMap (matchDate >=> getTime) (Date . Zoned)
  where
    getTime (Zoned z) = Right z
    getTime value     = leftMatchError $ Date value

-- | Local time bimap for 'AnyValue'. Usually used with 'localTime' combinator.
_LocalTime :: TomlBiMap LocalTime AnyValue
_LocalTime = mkAnyValueBiMap (matchDate >=> getTime) (Date . Local)
  where
    getTime (Local l) = Right l
    getTime value     = leftMatchError $ Date value

-- | Day bimap for 'AnyValue'. Usually used with 'day' combinator.
_Day :: TomlBiMap Day AnyValue
_Day = mkAnyValueBiMap (matchDate >=> getTime) (Date . Day)
  where
    getTime (Day d) = Right d
    getTime value   = leftMatchError $ Date value

-- | Time of day bimap for 'AnyValue'. Usually used with 'timeOfDay' combinator.
_TimeOfDay :: TomlBiMap TimeOfDay AnyValue
_TimeOfDay = mkAnyValueBiMap (matchDate >=> getTime) (Date . Hours)
  where
    getTime (Hours h) = Right h
    getTime value     = leftMatchError $ Date value

-- | Helper bimap for 'String' and 'Text'.
_StringText :: BiMap e String Text
_StringText = iso T.pack T.unpack

-- | 'String' bimap for 'AnyValue'. Usually used with 'string' combinator.
_String :: TomlBiMap String AnyValue
_String = _StringText >>> _Text

-- | Helper bimap for 'String' and types with 'Read' and 'Show' instances.
_ReadString :: (Show a, Read a) => TomlBiMap a String
_ReadString = BiMap (Right . show)
    (\value -> either (\_ -> leftWrongValue . Text $ tShow value) Right . readEither $ value)

-- | Bimap for 'AnyValue' and values with a `Read` and `Show` instance.
-- Usually used with 'read' combinator.
_Read :: (Show a, Read a) => TomlBiMap a AnyValue
_Read = _ReadString >>> _String

-- | Helper bimap for 'Natural' and 'Integer'.
_NaturalInteger :: TomlBiMap Natural Integer
_NaturalInteger = BiMap (Right . toInteger) eitherInteger
  where
    eitherInteger :: Integer -> Either TomlBiMapError Natural
    eitherInteger n
      | n < 0     = Left $ ArbitraryError "Value is below zero."
      | otherwise = Right (fromIntegral n)

-- | 'Natural' bimap for 'AnyValue'. Usually used with 'natural' combinator.
_Natural :: TomlBiMap Natural AnyValue
_Natural = _NaturalInteger >>> _Integer

-- | Helper bimap for 'Integer' and integral, bounded values.
_BoundedInteger :: (Integral a, Bounded a, Show a) => TomlBiMap a Integer
_BoundedInteger = BiMap (Right . toInteger) eitherBounded
  where
    eitherBounded :: forall a. (Integral a, Bounded a, Show a) => Integer -> Either TomlBiMapError a
    eitherBounded n
      | n < toInteger (minBound :: a) =
            Left $ ArbitraryError $ "Value " <> tShow n <> " is less minBound" <> tShow (minBound :: a)
      | n > toInteger (maxBound :: a) =
            Left $ ArbitraryError $ "Value " <> tShow n <> " is more maxBound" <> tShow (maxBound :: a)
      | otherwise                     = Right (fromIntegral n)

-- | 'Word' bimap for 'AnyValue'. Usually used with 'word' combinator.
_Word :: TomlBiMap Word AnyValue
_Word = _BoundedInteger >>> _Integer

-- | 'Int' bimap for 'AnyValue'. Usually used with 'int' combinator.
_Int :: TomlBiMap Int AnyValue
_Int = _BoundedInteger >>> _Integer

-- | 'Float' bimap for 'AnyValue'. Usually used with 'float' combinator.
_Float :: TomlBiMap Float AnyValue
_Float = iso realToFrac realToFrac >>> _Double

-- | Helper bimap for 'Text' and strict 'ByteString'
_ByteStringText :: TomlBiMap ByteString Text
_ByteStringText = prism T.encodeUtf8
    (\value -> either (\_ -> leftWrongValue . Text $ tShow value) Right $ T.decodeUtf8' value)

-- | 'ByteString' bimap for 'AnyValue'. Usually used with 'byteString' combinator.
_ByteString:: TomlBiMap ByteString AnyValue
_ByteString = _ByteStringText >>> _Text

-- | Helper bimap for 'Text' and lazy 'ByteString'
_LByteStringText :: TomlBiMap BL.ByteString Text
_LByteStringText = prism (TL.encodeUtf8 . TL.fromStrict) eitherText
  where
    eitherText :: BL.ByteString -> Either TomlBiMapError Text
    eitherText = either (\err -> Left $ ArbitraryError $ tShow err) (Right . TL.toStrict) . TL.decodeUtf8'

-- | Lazy 'ByteString' bimap for 'AnyValue'. Usually used with 'lazyByteString'
-- combinator.
_LByteString:: TomlBiMap BL.ByteString AnyValue
_LByteString = _LByteStringText >>> _Text

-- | Takes a bimap of a value and returns a bimap of a list of values and 'Anything'
-- as an array. Usually used with 'arrayOf' combinator.
_Array :: TomlBiMap a AnyValue -> TomlBiMap [a] AnyValue
_Array elementBimap = BiMap
    { forward = mapM (forward elementBimap) >=> fmap AnyValue . toTomlBiMapError . toMArray
    , backward = \(AnyValue val) -> toTomlBiMapError $ matchArray (toMatchError . backward elementBimap) val
    }

-- | Takes a bimap of a value and returns a bimap of a non-empty list of values
-- and 'Anything' as an array. Usually used with 'nonEmptyOf' combinator.
_NonEmpty :: a ~ Value t => TomlBiMap a AnyValue -> TomlBiMap (NE.NonEmpty a) AnyValue
_NonEmpty bimap = BiMap (Right . NE.toList)
    (\val -> maybe (leftWrongValue $ Array val) Right $ NE.nonEmpty val) >>> _Array bimap

-- | Takes a bimap of a value and returns a bimap of a set of values and 'Anything'
-- as an array. Usually used with 'setOf' combinator.
_Set :: (Ord a) => TomlBiMap a AnyValue -> TomlBiMap (S.Set a) AnyValue
_Set bimap = iso S.toList S.fromList >>> _Array bimap

-- | Takes a bimap of a value and returns a bimap of a has set of values and
-- 'Anything' as an array. Usually used with 'hashSetOf' combinator.
_HashSet :: (Eq a, Hashable a) => TomlBiMap a AnyValue -> TomlBiMap (HS.HashSet a) AnyValue
_HashSet bimap = iso HS.toList HS.fromList >>> _Array bimap

-- | Bimap of 'IntSet' and 'Anything' as an array. Usually used with
-- 'intSet' combinator.
_IntSet :: TomlBiMap IS.IntSet AnyValue
_IntSet = iso IS.toList IS.fromList >>> _Array _Int
