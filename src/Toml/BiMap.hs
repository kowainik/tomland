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
       , matchValueForward
       , mkAnyValueBiMap
       , _TextBy

         -- * Some predefined bi mappings
       , _Array
       , _Bool
       , _Double
       , _Integer
       , _Text
       , _TextToString
       , _String
       , _StringToShow
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

-- | Inverts bidirectional mapping.
invert :: BiMap a b -> BiMap b a
invert (BiMap f g) = BiMap g f

-- | Creates 'BiMap' from isomorphism.
iso :: (a -> b) -> (b -> a) -> BiMap a b
iso f g = BiMap (Just . f) (Just . g)

-- | Creates 'BiMap' from prism-like pair of functions.
prism :: (object -> Maybe field) -> (field -> object) -> BiMap object field
prism preview review = BiMap preview (Just . review)

----------------------------------------------------------------------------
-- General purpose bimaps
----------------------------------------------------------------------------

_Left :: BiMap l (Either l r)
_Left = invert $ prism (either Just (const Nothing)) Left

_Right :: BiMap r (Either l r)
_Right = invert $ prism (either (const Nothing) Just) Right

----------------------------------------------------------------------------
--  BiMaps for value
----------------------------------------------------------------------------

-- | Creates prism for 'AnyValue'.
mkAnyValueBiMap :: (forall t . Value t -> Maybe a)
                -> (a -> Value tag)
                -> BiMap AnyValue a
mkAnyValueBiMap matchValue toValue =
    prism (\(AnyValue value) -> matchValue value) (AnyValue . toValue)

-- | Creates prism for 'Text' to 'AnyValue' bimap with custom functions
_TextBy :: (Text -> Maybe a) -> (a -> Text) -> BiMap AnyValue a
_TextBy parseText toText =
  mkAnyValueBiMap (matchText >=> parseText) (Text . toText)

-- | Allows to match against given 'Value' using provided prism for 'AnyValue'.
matchValueForward :: BiMap AnyValue a -> Value t -> Maybe a
matchValueForward = liftMatch . forward

-- | 'Bool' bimap for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Bool :: BiMap AnyValue Bool
_Bool = mkAnyValueBiMap matchBool Bool

-- | 'Integer' bimap for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Integer :: BiMap AnyValue Integer
_Integer = mkAnyValueBiMap matchInteger Integer

-- | 'Double' bimap for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Double :: BiMap AnyValue Double
_Double = mkAnyValueBiMap matchDouble Double

-- | 'Text' bimap for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Text :: BiMap AnyValue Text
_Text = mkAnyValueBiMap matchText Text

_TextToString :: BiMap Text String
_TextToString = iso T.unpack T.pack

_String :: BiMap AnyValue String
_String = _Text >>> _TextToString

_StringToShow :: (Show a, Read a) => BiMap String a
_StringToShow = iso read show

_Show :: (Show a, Read a) => BiMap AnyValue a
_Show = _String >>> _StringToShow

-- | 'Array' bimap for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Array :: BiMap AnyValue a -> BiMap AnyValue [a]
_Array elementBimap = BiMap
    { forward  = \(AnyValue val) -> matchArray (forward elementBimap) val
    , backward = mapM (backward elementBimap) >=> fmap AnyValue . toMArray
    }

-- TODO: move somewhere else?
{- | Function for creating 'Array' from list of 'AnyValue'.
-}
toMArray :: [AnyValue] -> Maybe (Value 'TArray)
toMArray [] = Just $ Array []
toMArray (AnyValue x : xs) = case reifyAnyValues x xs of
    Left _     -> Nothing
    Right vals -> Just $ Array (x : vals)
