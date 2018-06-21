{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}

-- | Naive implementation of data-prism approach.

module Toml.Prism
       ( -- * Prism idea
         Prism (..)
       , match
       , mkAnyValuePrism

         -- * Value prisms
       , _Bool
       , _Integer
       , _Double
       , _String
       , _Array

         -- * Useful utility functions
       , unsafeArray
       ) where

import Control.Monad ((>=>))
import Data.Text (Text)

import Toml.Type (AnyValue (..), Value (..), ValueType (TArray), liftMatch, matchArray, matchBool,
                  matchDouble, matchInteger, matchText, reifyAnyValues)

import qualified Control.Category as Cat

----------------------------------------------------------------------------
-- Prism concepts and ideas
----------------------------------------------------------------------------

{- | Implementation of prism idea using simple data prism approach. Single value
of type 'Prism' has two capabilities:

1. 'preview': first-class pattern-matching (deconstruct @object@ to possible @field@).
2. 'review': constructor of @object@ from @field@.
-}
data Prism object field = Prism
    { preview :: object -> Maybe field
    , review  :: field -> object
    }

instance Cat.Category Prism where
    id :: Prism object object
    id = Prism { preview = Just, review = id }

    (.) :: Prism field subfield -> Prism object field -> Prism object subfield
    fieldPrism . objectPrism = Prism
        { preview = preview objectPrism >=> preview fieldPrism
        , review = review objectPrism . review fieldPrism
        }
    -- TODO: is naming too verbose?

-- | Creates prism for 'AnyValue'.
mkAnyValuePrism :: (forall t . Value t -> Maybe a)
                -> (a -> Value tag)
                -> Prism AnyValue a
mkAnyValuePrism matchValue toValue = Prism
    { review = AnyValue . toValue
    , preview = \(AnyValue value) -> matchValue value
    }

-- | Allows to match against given 'Value' using provided prism for 'AnyValue'.
match :: Prism AnyValue a -> Value t -> Maybe a
match = liftMatch . preview

----------------------------------------------------------------------------
--  Prisms for value
----------------------------------------------------------------------------

-- | 'Bool' prism for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Bool :: Prism AnyValue Bool
_Bool = mkAnyValuePrism matchBool Bool

-- | 'Integer' prism for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Integer :: Prism AnyValue Integer
_Integer = mkAnyValuePrism matchInteger Int

-- | 'Double' prism for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Double :: Prism AnyValue Double
_Double = mkAnyValuePrism matchDouble Float

-- | 'Text' prism for 'AnyValue'. Usually used with 'arrayOf' combinator.
_String :: Prism AnyValue Text
_String = mkAnyValuePrism matchText String

-- | 'Array' prism for 'AnyValue'. Usually used with 'arrayOf' combinator.
_Array :: Prism AnyValue a -> Prism AnyValue [a]
_Array elementPrism = mkAnyValuePrism (matchArray $ preview elementPrism)
                                      (unsafeArray . map (review elementPrism))

-- TODO: put this in 'Toml.Type' module?
-- | Unsafe function for creating 'Array' from list of 'AnyValue'. This function
-- assumes that every element in this list has the same type. Usually used when
-- list of 'AnyValue' is created using single prism.
unsafeArray :: [AnyValue] -> Value 'TArray
unsafeArray [] = Array []
unsafeArray (AnyValue x : xs) = case reifyAnyValues x xs of
    Left err   -> error $ "Can't create Array from list AnyValues: " ++ show err
    Right vals -> Array (x : vals)
