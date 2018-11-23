{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}

{- | Implementation of partial bidirectional mapping as a data type.
-}

module Toml.BiMap
       ( -- * BiMap
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

         -- * Generic BiMap
       , GenBiMap (..)
       , _Generic
       ) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))

import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Word (Word)
import GHC.Generics
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

import Toml.Type (AnyValue (..), DateTime (..), Value (..), matchArray, matchBool, matchDate,
                  matchDouble, matchInteger, matchText, toMArray)

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

1. @a -> Maybe b@
2. @b -> Maybe a@
-}
data BiMap a b = BiMap
    { forward  :: a -> Maybe b
    , backward :: b -> Maybe a
    }

instance Cat.Category BiMap where
    id :: BiMap a a
    id = BiMap Just Just

    (.) :: BiMap b c -> BiMap a b -> BiMap a c
    bc . ab = BiMap
        { forward  =  forward ab >=>  forward bc
        , backward = backward bc >=> backward ab
        }

-- | Inverts bidirectional mapping.
invert :: BiMap a b -> BiMap b a
invert (BiMap f g) = BiMap g f

-- | Creates 'BiMap' from isomorphism.
iso :: (a -> b) -> (b -> a) -> BiMap a b
iso f g = BiMap (Just . f) (Just . g)

-- | Creates 'BiMap' from prism-like pair of functions.
prism :: (field -> object) -> (object -> Maybe field) -> BiMap object field
prism review preview = BiMap preview (Just . review)

----------------------------------------------------------------------------
-- General purpose bimaps
----------------------------------------------------------------------------

-- | Bimap for 'Either' and its left type
_Left :: BiMap (Either l r) l
_Left = prism Left (either Just (const Nothing))

-- | Bimap for 'Either' and its right type
_Right :: BiMap (Either l r) r
_Right = prism Right (either (const Nothing) Just)

-- | Bimap for 'Maybe'
_Just :: BiMap (Maybe a) a
_Just = prism Just id

----------------------------------------------------------------------------
--  BiMaps for value
----------------------------------------------------------------------------

-- | Creates prism for 'AnyValue'.
mkAnyValueBiMap :: (forall t . Value t -> Maybe a)
                -> (a -> Value tag)
                -> BiMap a AnyValue
mkAnyValueBiMap matchValue toValue =
    BiMap (Just . AnyValue . toValue) (\(AnyValue value) -> matchValue value)

-- | Creates bimap for 'Text' to 'AnyValue' with custom functions
_TextBy :: (a -> Text) -> (Text -> Maybe a) -> BiMap a AnyValue
_TextBy toText parseText =
  mkAnyValueBiMap (matchText >=> parseText) (Text . toText)

-- | 'Bool' bimap for 'AnyValue'. Usually used with 'bool' combinator.
_Bool :: BiMap Bool AnyValue
_Bool = mkAnyValueBiMap matchBool Bool

-- | 'Integer' bimap for 'AnyValue'. Usually used with 'integer' combinator.
_Integer :: BiMap Integer AnyValue
_Integer = mkAnyValueBiMap matchInteger Integer

-- | 'Double' bimap for 'AnyValue'. Usually used with 'double' combinator.
_Double :: BiMap Double AnyValue
_Double = mkAnyValueBiMap matchDouble Double

-- | 'Text' bimap for 'AnyValue'. Usually used with 'text' combinator.
_Text :: BiMap Text AnyValue
_Text = mkAnyValueBiMap matchText Text

-- | Zoned time bimap for 'AnyValue'. Usually used with 'zonedTime' combinator.
_ZonedTime :: BiMap ZonedTime AnyValue
_ZonedTime = mkAnyValueBiMap (matchDate >=> getTime) (Date . Zoned)
  where
    getTime (Zoned z) = Just z
    getTime _         = Nothing

-- | Local time bimap for 'AnyValue'. Usually used with 'localTime' combinator.
_LocalTime :: BiMap LocalTime AnyValue
_LocalTime = mkAnyValueBiMap (matchDate >=> getTime) (Date . Local)
  where
    getTime (Local l) = Just l
    getTime _         = Nothing

-- | Day bimap for 'AnyValue'. Usually used with 'day' combinator.
_Day :: BiMap Day AnyValue
_Day = mkAnyValueBiMap (matchDate >=> getTime) (Date . Day)
  where
    getTime (Day d) = Just d
    getTime _       = Nothing

-- | Time of day bimap for 'AnyValue'. Usually used with 'timeOfDay' combinator.
_TimeOfDay :: BiMap TimeOfDay AnyValue
_TimeOfDay = mkAnyValueBiMap (matchDate >=> getTime) (Date . Hours)
  where
    getTime (Hours h) = Just h
    getTime _         = Nothing

-- | Helper bimap for 'String' and 'Text'.
_StringText :: BiMap String Text
_StringText = iso T.pack T.unpack

-- | 'String' bimap for 'AnyValue'. Usually used with 'string' combinator.
_String :: BiMap String AnyValue
_String = _StringText >>> _Text

-- | Helper bimap for 'String' and types with 'Read' and 'Show' instances.
_ReadString :: (Show a, Read a) => BiMap a String
_ReadString = BiMap (Just . show) readMaybe

-- | Bimap for 'AnyValue' and values with a `Read` and `Show` instance.
-- Usually used with 'read' combinator.
_Read :: (Show a, Read a) => BiMap a AnyValue
_Read = _ReadString >>> _String

-- | Helper bimap for 'Natural' and 'Integer'.
_NaturalInteger :: BiMap Natural Integer
_NaturalInteger = BiMap (Just . toInteger) maybeInteger
  where
    maybeInteger :: Integer -> Maybe Natural
    maybeInteger n
      | n < 0     = Nothing
      | otherwise = Just (fromIntegral n)

-- | 'Natural' bimap for 'AnyValue'. Usually used with 'natural' combinator.
_Natural :: BiMap Natural AnyValue
_Natural = _NaturalInteger >>> _Integer

-- | Helper bimap for 'Integer' and integral, bounded values.
_BoundedInteger :: (Integral a, Bounded a) => BiMap a Integer
_BoundedInteger = BiMap (Just . toInteger) maybeBounded
  where
    maybeBounded :: forall a. (Integral a, Bounded a) => Integer -> Maybe a
    maybeBounded n
      | n < toInteger (minBound :: a) = Nothing
      | n > toInteger (maxBound :: a) = Nothing
      | otherwise                     = Just (fromIntegral n)

-- | 'Word' bimap for 'AnyValue'. Usually used with 'word' combinator.
_Word :: BiMap Word AnyValue
_Word = _BoundedInteger >>> _Integer

-- | 'Int' bimap for 'AnyValue'. Usually used with 'int' combinator.
_Int :: BiMap Int AnyValue
_Int = _BoundedInteger >>> _Integer

-- | 'Float' bimap for 'AnyValue'. Usually used with 'float' combinator.
_Float :: BiMap Float AnyValue
_Float = iso realToFrac realToFrac >>> _Double

-- | Helper bimap for 'Text' and strict 'ByteString'
_ByteStringText :: BiMap ByteString Text
_ByteStringText = prism T.encodeUtf8 maybeText
  where
    maybeText :: ByteString -> Maybe Text
    maybeText = either (const Nothing) Just . T.decodeUtf8'

-- | 'ByteString' bimap for 'AnyValue'. Usually used with 'byteString' combinator.
_ByteString:: BiMap ByteString AnyValue
_ByteString = _ByteStringText >>> _Text

-- | Helper bimap for 'Text' and lazy 'ByteString'
_LByteStringText :: BiMap BL.ByteString Text
_LByteStringText = prism (TL.encodeUtf8 . TL.fromStrict) maybeText
  where
    maybeText :: BL.ByteString -> Maybe Text
    maybeText = either (const Nothing) (Just . TL.toStrict) . TL.decodeUtf8'

-- | Lazy 'ByteString' bimap for 'AnyValue'. Usually used with 'lazyByteString'
-- combinator.
_LByteString:: BiMap BL.ByteString AnyValue
_LByteString = _LByteStringText >>> _Text

-- | Takes a bimap of a value and returns a bimap of a list of values and 'Anything'
-- as an array. Usually used with 'arrayOf' combinator.
_Array :: BiMap a AnyValue -> BiMap [a] AnyValue
_Array elementBimap = BiMap
    { forward = mapM (forward elementBimap) >=> fmap AnyValue . toMArray
    , backward = \(AnyValue val) -> matchArray (backward elementBimap) val
    }

-- | Takes a bimap of a value and returns a bimap of a non-empty list of values
-- and 'Anything' as an array. Usually used with 'nonEmptyOf' combinator.
_NonEmpty :: BiMap a AnyValue -> BiMap (NE.NonEmpty a) AnyValue
_NonEmpty bimap = BiMap (Just . NE.toList) NE.nonEmpty >>> _Array bimap

-- | Takes a bimap of a value and returns a bimap of a set of values and 'Anything'
-- as an array. Usually used with 'setOf' combinator.
_Set :: (Ord a) => BiMap a AnyValue -> BiMap (S.Set a) AnyValue
_Set bimap = iso S.toList S.fromList >>> _Array bimap

-- | Takes a bimap of a value and returns a bimap of a has set of values and
-- 'Anything' as an array. Usually used with 'hashSetOf' combinator.
_HashSet :: (Eq a, Hashable a) => BiMap a AnyValue -> BiMap (HS.HashSet a) AnyValue
_HashSet bimap = iso HS.toList HS.fromList >>> _Array bimap

-- | Bimap of 'IntSet' and 'Anything' as an array. Usually used with
-- 'intSet' combinator.
_IntSet :: BiMap IS.IntSet AnyValue
_IntSet = iso IS.toList IS.fromList >>> _Array _Int

----------------------------------------------------------------------------
-- Generic data structures
----------------------------------------------------------------------------

-- | Class to generate a 'BiMap' from any generic data structure to 'Anything'.
class GenBiMap a where
  -- | Encoding the data structure
  _encode :: a -> Maybe AnyValue
  -- | Adding extra constraints
  default _encode :: (Generic a, GenBiMap' (Rep a)) => a -> Maybe AnyValue
  _encode = encode' . from

  -- | Decoding the data structure
  _decode :: AnyValue -> Maybe a
  -- | Adding extra constraints
  default _decode :: (Generic a, GenBiMap' (Rep a)) => AnyValue -> Maybe a
  _decode = fmap to . decode'

-- | Helper class needed to implement 'GenBiMap'.
class GenBiMap' f where
  encode' :: f p -> Maybe AnyValue
  decode' :: AnyValue -> Maybe (f p)

-- | Instance for for datatypes without constructors.
instance GenBiMap' V1 where
  encode' _ = undefined
  decode' _ = undefined

-- | Instance for constructors without arguents.
instance GenBiMap' U1 where
  encode' U1 = Just $ AnyValue $ Array []

  decode' (AnyValue (Array [])) = Just U1
  decode' _                     = Nothing

-- | Instance for sum types.
instance (GenBiMap' f, GenBiMap' g) => GenBiMap' (f :+: g) where
  encode' = \case
      (L1 x) -> encode' x >>= go False
      (R1 x) -> encode' x >>= go True
    where
      go :: Bool -> AnyValue -> Maybe AnyValue
      go b (AnyValue v) = Just $ AnyValue $ Array [Array [Bool b], Array [v]]

  decode' (AnyValue (Array [Array [Bool False], Array [v]])) =
               L1 <$> decode' (AnyValue v)
  decode' (AnyValue (Array [Array [Bool  True], Array [v]])) =
               R1 <$> decode' (AnyValue v)
  decode' _ = Nothing

-- | Instance for product types.
instance (GenBiMap' f, GenBiMap' g) => GenBiMap' (f :*: g) where
  encode' (x :*: y) = go <$> encode' x <*> encode' y
    where
      go :: AnyValue -> AnyValue -> AnyValue
      go (AnyValue v) (AnyValue w) = AnyValue $ Array [Array [v], Array [w]]

  decode' (AnyValue (Array [Array [v], Array [w]])) =
    (:*:) <$> decode' (AnyValue v) <*> decode' (AnyValue w)
  decode' _ = Nothing

-- | Instance for constants, additional parameters and recursion of kind *.
instance (GenBiMap c) => GenBiMap' (K1 i c) where
  encode' (K1 x) = _encode x
  decode' v = K1 <$> _decode v

-- | Instance for Meta-information (constructor names, etc.).
instance (GenBiMap' f) => GenBiMap' (M1 i t f) where
  encode' (M1 x) = encode' x
  decode' v = M1 <$> decode' v

-- | Shortcut instance for 'AnyValue'.
instance GenBiMap AnyValue where
  _encode = Just
  _decode = Just

-- | Shortcut instance for 'Integer'.
instance GenBiMap Integer where
  _encode = forward _Integer
  _decode = backward _Integer

-- | Shortcut instance for 'Int'.
instance GenBiMap Int where
  _encode = forward _Int
  _decode = backward _Int

-- | Shortcut instance for 'Bool'.
instance GenBiMap Bool where
  _encode = forward _Bool
  _decode = backward _Bool

-- | Shortcut instance for 'Text'.
instance GenBiMap Text where
  _encode = forward _Text
  _decode = backward _Text

-- | Shortcut instance for 'Double'.
instance GenBiMap Double where
  _encode = forward _Double
  _decode = backward _Double

-- | Shortcut instance for 'Day'.
instance GenBiMap Day where
  _encode = forward _Day
  _decode = backward _Day

-- | Shortcut instance for 'ZonedTime'.
instance GenBiMap ZonedTime where
  _encode = forward _ZonedTime
  _decode = backward _ZonedTime

-- | Shortcut instance for 'TimeOfDay'.
instance GenBiMap TimeOfDay where
  _encode = forward _TimeOfDay
  _decode = backward _TimeOfDay

-- | Shortcut instance for 'LocalTime'.
instance GenBiMap LocalTime where
  _encode = forward _LocalTime
  _decode = backward _LocalTime

-- | Shortcut instance for lists.
instance GenBiMap a => GenBiMap [a] where
  _encode = forward  $ _Array _Generic
  _decode = backward $ _Array _Generic

-- | Instance for Unit.
instance GenBiMap ()

-- | Instance for 'Either'.
instance (GenBiMap a, GenBiMap b) => GenBiMap (Either a b)

-- | Instance for pairs.
instance (GenBiMap a, GenBiMap b) => GenBiMap (a, b)

-- | Instance for triples.
instance (GenBiMap a, GenBiMap b, GenBiMap c) => GenBiMap (a, b, c)

-- | Instance for quadruples.
instance (GenBiMap a, GenBiMap b, GenBiMap c, GenBiMap d) => GenBiMap (a, b, c, d)

-- | Generic 'BiMap' from any data structure to 'AnyValue'.
_Generic :: GenBiMap a => BiMap a AnyValue
_Generic = BiMap _encode _decode
