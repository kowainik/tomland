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
       , matchValueBackward
       , mkAnyValueBiMap
       , _TextBy

         -- * Some predefined bi mappings
       , _Array
       , _Bool
       , _Double
       , _Integer
       , _Text
       , _StringText
       , _String
       , _ShowString
       , _Show
       , _NaturalInteger
       , _Natural
       , _WordInteger
       , _Word
       , _Float
       , _ByteStringText
       , _ByteString
       , _LByteStringText
       , _LByteString

       , _Left
       , _Right
       , _Just

         -- * Useful utility functions
       , toMArray
       ) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

import Toml.Type (AnyValue (..), TValue (TArray), Value (..), liftMatch, matchArray, matchBool,
                  matchDouble, matchInteger, matchText, reifyAnyValues)

import qualified Control.Category as Cat
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
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

_Left :: BiMap (Either l r) l
_Left = prism Left (either Just (const Nothing))

_Right :: BiMap (Either l r) r
_Right = prism Right (either (const Nothing) Just)

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

-- | Allows to match against given 'Value' using provided prism for 'AnyValue'.
matchValueBackward :: BiMap a AnyValue -> Value t -> Maybe a
matchValueBackward = liftMatch . backward

-- | Creates prism for 'Text' to 'AnyValue' with custom functions
_TextBy :: (a -> Text) -> (Text -> Maybe a) -> BiMap a AnyValue
_TextBy toText parseText =
  mkAnyValueBiMap (matchText >=> parseText) (Text . toText)

-- | 'Bool' bimap for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Bool :: BiMap Bool AnyValue
_Bool = mkAnyValueBiMap matchBool Bool

-- | 'Integer' bimap for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Integer :: BiMap Integer AnyValue
_Integer = mkAnyValueBiMap matchInteger Integer

-- | 'Double' bimap for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Double :: BiMap Double AnyValue
_Double = mkAnyValueBiMap matchDouble Double

-- | 'Text' bimap for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Text :: BiMap Text AnyValue
_Text = mkAnyValueBiMap matchText Text

_StringText :: BiMap String Text
_StringText = iso T.pack T.unpack

_String :: BiMap String AnyValue
_String = _StringText >>> _Text

_ShowString :: (Show a, Read a) => BiMap a String
_ShowString = BiMap (Just . show) readMaybe

_Show :: (Show a, Read a) => BiMap a AnyValue
_Show = _ShowString >>> _String

_NaturalInteger :: BiMap Natural Integer
_NaturalInteger = BiMap (Just . toInteger) maybeInteger
  where maybeInteger n
          | n < 0     = Nothing
          | otherwise = Just (fromIntegral n)

_Natural :: BiMap Natural AnyValue
_Natural = _NaturalInteger >>> _Integer

_WordInteger :: BiMap Word Integer
_WordInteger = BiMap (Just . toInteger) maybeInteger
  where maybeInteger n
          | n < toInteger (minBound :: Word) = Nothing
          | n > toInteger (maxBound :: Word) = Nothing
          | otherwise                        = Just (fromIntegral n)

_Word :: BiMap Word AnyValue
_Word = _WordInteger >>> _Integer

_Float :: BiMap Float AnyValue
_Float = iso realToFrac realToFrac >>> _Double

_ByteStringText :: BiMap ByteString Text
_ByteStringText = prism T.encodeUtf8 maybeText
  where maybeText = either (const Nothing) Just . T.decodeUtf8'

_ByteString:: BiMap ByteString AnyValue
_ByteString = _ByteStringText >>> _Text

_LByteStringText :: BiMap BL.ByteString Text
_LByteStringText = prism (TL.encodeUtf8 . TL.fromStrict) maybeText
  where maybeText = either (const Nothing) (Just . TL.toStrict) . TL.decodeUtf8'

_LByteString:: BiMap BL.ByteString AnyValue
_LByteString = _LByteStringText >>> _Text

-- | 'Array' bimap for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Array :: BiMap a AnyValue -> BiMap [a] AnyValue
_Array elementBimap = BiMap
    { forward = mapM (forward elementBimap) >=> fmap AnyValue . toMArray
    , backward = \(AnyValue val) -> matchArray (backward elementBimap) val
    }

-- TODO: move somewhere else?
{- | Function for creating 'Array' from list of 'AnyValue'.
-}
toMArray :: [AnyValue] -> Maybe (Value 'TArray)
toMArray [] = Just $ Array []
toMArray (AnyValue x : xs) = case reifyAnyValues x xs of
    Left _     -> Nothing
    Right vals -> Just $ Array (x : vals)
