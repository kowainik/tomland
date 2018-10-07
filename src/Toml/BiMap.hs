{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}

{- | Implementation of partial bidirectional mapping as a data type.
-}

module Toml.BiMap
       ( -- * BiMap idea
         BiMap (..)
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
       , _StringToText
       , _String
       , _ShowToString
       , _Show

       , _Left
       , _Right

         -- * Useful utility functions
       , toMArray
       ) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Text (Text)

import Toml.Type (AnyValue (..), TValue (TArray), Value (..), liftMatch, matchArray, matchBool,
                  matchDouble, matchInteger, matchText, reifyAnyValues)

import qualified Control.Category as Cat
import qualified Data.Text as T


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

-- | Creates 'BiMap' from isomorphism.
iso :: (a -> b) -> (b -> a) -> BiMap a b
iso f g = BiMap (Just . f) (Just . g)

-- | Creates 'BiMap' from prism-like pair of functions.
prism :: (object -> Maybe field) -> (field -> object) -> BiMap object field
prism preview review = BiMap preview (Just . review)

----------------------------------------------------------------------------
-- General purpose bimaps
----------------------------------------------------------------------------

_Left :: BiMap (Either l r) l
_Left = prism (either Just (const Nothing)) Left

_Right :: BiMap (Either l r) r
_Right = prism (either (const Nothing) Just) Right

----------------------------------------------------------------------------
--  BiMaps for value
----------------------------------------------------------------------------

-- | Creates prism for 'AnyValue'.
mkAnyValueBiMap :: (forall t . Value t -> Maybe a)
                -> (a -> Value tag)
                -> BiMap a AnyValue
mkAnyValueBiMap matchValue toValue =
    BiMap (Just . AnyValue . toValue) (\(AnyValue value) -> matchValue value)

-- | Creates prism for 'Text' to 'AnyValue' with custom functions
_TextBy :: (Text -> Maybe a) -> (a -> Text) -> BiMap a AnyValue
_TextBy parseText toText =
  mkAnyValueBiMap (matchText >=> parseText) (Text . toText)

-- | Allows to match against given 'Value' using provided prism for 'AnyValue'.
matchValueBackward :: BiMap a AnyValue -> Value t -> Maybe a
matchValueBackward = liftMatch . backward

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

_StringToText :: BiMap String Text
_StringToText = iso T.pack T.unpack

_String :: BiMap String AnyValue
_String = _StringToText >>> _Text

_ShowToString :: (Show a, Read a) => BiMap a String
_ShowToString = BiMap (Just . show) (Just . read)

_Show :: (Show a, Read a) => BiMap a AnyValue
_Show = _ShowToString >>> _String

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
