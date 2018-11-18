{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}

{- | Implementation of partial bidirectional mapping as a data type.
-}

module Toml.BiMap
       ( -- * BiMap idea
         BiMap (..)
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
       ) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))

import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Word (Word)
import Numeric.Natural (Natural)
import Text.Read (readEither)

import Toml.Type (AnyValue (..), BiMapError (..), DateTime (..), Value (..), matchArray, matchBool,
                  matchDate, matchDouble, matchInteger, matchText, toMArray)

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
-- General purpose bimaps
----------------------------------------------------------------------------

-- | Bimap for 'Either' and its left type
_Left :: BiMap BiMapError (Either l r) l
_Left = prism Left (either Right (const $ Left BiMapError))

-- | Bimap for 'Either' and its right type
_Right :: BiMap BiMapError (Either l r) r
_Right = prism Right (either (const $ Left BiMapError) Right)

-- | Bimap for 'Maybe'
_Just :: BiMap BiMapError (Maybe r) r
_Just = prism Just (maybe (Left BiMapError) Right)

----------------------------------------------------------------------------
--  BiMaps for value
----------------------------------------------------------------------------

-- | Creates prism for 'AnyValue'.
mkAnyValueBiMap :: (forall t . Value t -> Either BiMapError a)
                -> (a -> Value tag)
                -> BiMap BiMapError a AnyValue
mkAnyValueBiMap matchValue toValue =
    BiMap (Right . AnyValue . toValue) (\(AnyValue value) -> matchValue value)

-- | Creates bimap for 'Text' to 'AnyValue' with custom functions
_TextBy :: (a -> Text) -> (Text -> Either BiMapError a) -> BiMap BiMapError a AnyValue
_TextBy toText parseText =
  mkAnyValueBiMap (matchText >=> parseText) (Text . toText)

-- | 'Bool' bimap for 'AnyValue'. Usually used with 'bool' combinator.
_Bool :: BiMap BiMapError Bool AnyValue
_Bool = mkAnyValueBiMap matchBool Bool

-- | 'Integer' bimap for 'AnyValue'. Usually used with 'integer' combinator.
_Integer :: BiMap BiMapError Integer AnyValue
_Integer = mkAnyValueBiMap matchInteger Integer

-- | 'Double' bimap for 'AnyValue'. Usually used with 'double' combinator.
_Double :: BiMap BiMapError Double AnyValue
_Double = mkAnyValueBiMap matchDouble Double

-- | 'Text' bimap for 'AnyValue'. Usually used with 'text' combinator.
_Text :: BiMap BiMapError Text AnyValue
_Text = mkAnyValueBiMap matchText Text

-- | Zoned time bimap for 'AnyValue'. Usually used with 'zonedTime' combinator.
_ZonedTime :: BiMap BiMapError ZonedTime AnyValue
_ZonedTime = mkAnyValueBiMap (matchDate >=> getTime) (Date . Zoned)
  where
    getTime (Zoned z) = Right z
    getTime _         = Left BiMapError

-- | Local time bimap for 'AnyValue'. Usually used with 'localTime' combinator.
_LocalTime :: BiMap BiMapError LocalTime AnyValue
_LocalTime = mkAnyValueBiMap (matchDate >=> getTime) (Date . Local)
  where
    getTime (Local l) = Right l
    getTime _         = Left BiMapError

-- | Day bimap for 'AnyValue'. Usually used with 'day' combinator.
_Day :: BiMap BiMapError Day AnyValue
_Day = mkAnyValueBiMap (matchDate >=> getTime) (Date . Day)
  where
    getTime (Day d) = Right d
    getTime _       = Left BiMapError

-- | Time of day bimap for 'AnyValue'. Usually used with 'timeOfDay' combinator.
_TimeOfDay :: BiMap BiMapError TimeOfDay AnyValue
_TimeOfDay = mkAnyValueBiMap (matchDate >=> getTime) (Date . Hours)
  where
    getTime (Hours h) = Right h
    getTime _         = Left BiMapError

-- | Helper bimap for 'String' and 'Text'.
_StringText :: BiMap e String Text
_StringText = iso T.pack T.unpack

-- | 'String' bimap for 'AnyValue'. Usually used with 'string' combinator.
_String :: BiMap BiMapError String AnyValue
_String = _StringText >>> _Text

-- | Helper bimap for 'String' and types with 'Read' and 'Show' instances.
_ReadString :: (Show a, Read a) => BiMap BiMapError a String
_ReadString = BiMap (Right . show) (either (const $ Left BiMapError) Right . readEither)

-- | Bimap for 'AnyValue' and values with a `Read` and `Show` instance.
-- Usually used with 'read' combinator.
_Read :: (Show a, Read a) => BiMap BiMapError a AnyValue
_Read = _ReadString >>> _String

-- | Helper bimap for 'Natural' and 'Integer'.
_NaturalInteger :: BiMap BiMapError Natural Integer
_NaturalInteger = BiMap (Right . toInteger) eitherInteger
  where
    eitherInteger :: Integer -> Either BiMapError Natural
    eitherInteger n
      | n < 0     = Left BiMapError
      | otherwise = Right (fromIntegral n)

-- | 'Natural' bimap for 'AnyValue'. Usually used with 'natural' combinator.
_Natural :: BiMap BiMapError Natural AnyValue
_Natural = _NaturalInteger >>> _Integer

-- | Helper bimap for 'Integer' and integral, bounded values.
_BoundedInteger :: (Integral a, Bounded a) => BiMap BiMapError a Integer
_BoundedInteger = BiMap (Right . toInteger) (either (\_ -> Left BiMapError) Right . eitherBounded)
  where
    eitherBounded :: forall a. (Integral a, Bounded a) => Integer -> Either BiMapError a
    eitherBounded n
      | n < toInteger (minBound :: a) = Left BiMapError
      | n > toInteger (maxBound :: a) = Left BiMapError
      | otherwise                     = Right (fromIntegral n)

-- | 'Word' bimap for 'AnyValue'. Usually used with 'word' combinator.
_Word :: BiMap BiMapError Word AnyValue
_Word = _BoundedInteger >>> _Integer

-- | 'Int' bimap for 'AnyValue'. Usually used with 'int' combinator.
_Int :: BiMap BiMapError Int AnyValue
_Int = _BoundedInteger >>> _Integer

-- | 'Float' bimap for 'AnyValue'. Usually used with 'float' combinator.
_Float :: BiMap BiMapError Float AnyValue
_Float = iso realToFrac realToFrac >>> _Double

-- | Helper bimap for 'Text' and strict 'ByteString'
_ByteStringText :: BiMap BiMapError ByteString Text
_ByteStringText = prism T.encodeUtf8 eitherText
  where
    eitherText :: ByteString -> Either BiMapError Text
    eitherText = either (const $ Left BiMapError) Right . T.decodeUtf8'

-- | 'ByteString' bimap for 'AnyValue'. Usually used with 'byteString' combinator.
_ByteString:: BiMap BiMapError ByteString AnyValue
_ByteString = _ByteStringText >>> _Text

-- | Helper bimap for 'Text' and lazy 'ByteString'
_LByteStringText :: BiMap BiMapError BL.ByteString Text
_LByteStringText = prism (TL.encodeUtf8 . TL.fromStrict) eitherText
  where
    eitherText :: BL.ByteString -> Either BiMapError Text
    eitherText bs = TL.toStrict <$> either (const $ Left BiMapError) Right (TL.decodeUtf8' bs)

-- | Lazy 'ByteString' bimap for 'AnyValue'. Usually used with 'lazyByteString'
-- combinator.
_LByteString:: BiMap BiMapError BL.ByteString AnyValue
_LByteString = _LByteStringText >>> _Text

-- | Takes a bimap of a value and returns a bimap of a list of values and 'Anything'
-- as an array. Usually used with 'arrayOf' combinator.
_Array :: BiMap BiMapError a AnyValue -> BiMap BiMapError [a] AnyValue
_Array elementBimap = BiMap
    { forward = mapM (forward elementBimap) >=> fmap AnyValue . toMArray
    , backward = \(AnyValue val) -> matchArray (backward elementBimap) val
    }

-- | Takes a bimap of a value and returns a bimap of a non-empty list of values
-- and 'Anything' as an array. Usually used with 'nonEmptyOf' combinator.
_NonEmpty :: BiMap BiMapError a AnyValue -> BiMap BiMapError (NE.NonEmpty a) AnyValue
_NonEmpty bimap = BiMap (Right . NE.toList)
    (maybe (Left BiMapError) Right . NE.nonEmpty) >>> _Array bimap

-- | Takes a bimap of a value and returns a bimap of a set of values and 'Anything'
-- as an array. Usually used with 'setOf' combinator.
_Set :: (Ord a) => BiMap BiMapError a AnyValue -> BiMap BiMapError (S.Set a) AnyValue
_Set bimap = iso S.toList S.fromList >>> _Array bimap

-- | Takes a bimap of a value and returns a bimap of a has set of values and
-- 'Anything' as an array. Usually used with 'hashSetOf' combinator.
_HashSet :: (Eq a, Hashable a) => BiMap BiMapError a AnyValue -> BiMap BiMapError (HS.HashSet a) AnyValue
_HashSet bimap = iso HS.toList HS.fromList >>> _Array bimap

-- | Bimap of 'IntSet' and 'Anything' as an array. Usually used with
-- 'intSet' combinator.
_IntSet :: BiMap BiMapError IS.IntSet AnyValue
_IntSet = iso IS.toList IS.fromList >>> _Array _Int
