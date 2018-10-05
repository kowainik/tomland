{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeFamilies     #-}

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
       -- , mkAnyTypeBiMap

         -- * Some predefined bi mappings
       , _Array
       , _Bool
       , _Double
       , _Integer
       , _String
       , _Text
       , _TextToString

       , _Left
       , _Right

         -- * Useful utility functions
       , toMArray
       ) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Text (Text)

import Toml.Type (AnyValue (..), TValue (..), DateTime (..), Value (..), liftMatch, matchArray, matchBool,
                  matchDouble, matchInteger, matchText, matchDate, reifyAnyValues)

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
mkAnyValueBiMap matchValue e =
    prism (\(AnyValue value) -> matchValue value) (AnyValue . e)

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

-- | Used to create prisms for 'AnyValue' from original Types.
class Valuable m where
  toValue :: m -> AnyValue
  matchValue :: AnyValue -> Maybe m

instance Valuable Bool where
  toValue = AnyValue . Bool
  matchValue (AnyValue t) = matchBool t

instance Valuable Integer where
  toValue = AnyValue . Integer
  matchValue (AnyValue t) = matchInteger t

instance Valuable Double where
  toValue = AnyValue . Double
  matchValue (AnyValue t) = matchDouble t

instance Valuable Text where
  toValue = AnyValue . Text
  matchValue (AnyValue t) = matchText t

instance Valuable DateTime where
  toValue = AnyValue . Date
  matchValue (AnyValue t) = matchDate t

-- | Creates prism for 'AnyValue' from original Type.
mkAnyTypeBiMap :: Valuable m => (a -> m) -> (m -> Maybe a) -> BiMap AnyValue a
mkAnyTypeBiMap showA parseA = BiMap
        { forward = \ anyVal -> matchValue anyVal >>= parseA
        , backward = Just . toValue . showA
        }
