{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

module Toml.Type.AnyValue
       ( AnyValue (..)
       , reifyAnyValues

         -- * Matching
       , liftMatch
       , matchBool
       , matchInteger
       , matchDouble
       , matchText
       , matchDate
       , matchArray
       ) where

import Data.Text (Text)
import Data.Type.Equality ((:~:) (..))

import Toml.Type.Value (DateTime, TValue, TypeMismatchError, Value (..), eqValueList, sameValue)

-- | Existential wrapper for 'Value'.
data AnyValue = forall (t :: TValue) . AnyValue (Value t)

instance Show AnyValue where
    show (AnyValue v) = show v

instance Eq AnyValue where
    (AnyValue (Bool b1))    == (AnyValue (Bool b2))    = b1 == b2
    (AnyValue (Integer i1)) == (AnyValue (Integer i2)) = i1 == i2
    (AnyValue (Double f1))  == (AnyValue (Double f2))  = f1 == f2
    (AnyValue (Text s1))    == (AnyValue (Text s2))    = s1 == s2
    (AnyValue (Date d1))    == (AnyValue (Date d2))    = d1 == d2
    (AnyValue (Array a1))   == (AnyValue (Array a2))   = eqValueList a1 a2
    _                       == _                       = False

----------------------------------------------------------------------------
-- Matching functions for values
----------------------------------------------------------------------------

-- | Extract 'Bool' from 'Value'.
matchBool :: Value t -> Maybe Bool
matchBool (Bool b) = Just b
matchBool _        = Nothing

-- | Extract 'Integer' from 'Value'.
matchInteger :: Value t -> Maybe Integer
matchInteger (Integer n) = Just n
matchInteger _           = Nothing

-- | Extract 'Double' from 'Value'.
matchDouble :: Value t -> Maybe Double
matchDouble (Double f) = Just f
matchDouble _          = Nothing

-- | Extract 'Text' from 'Value'.
matchText :: Value t -> Maybe Text
matchText (Text s) = Just s
matchText _        = Nothing

-- | Extract 'DateTime' from 'Value'.
matchDate :: Value t -> Maybe DateTime
matchDate (Date d) = Just d
matchDate _        = Nothing

-- | Extract list of elements of type @a@ from array.
matchArray :: (AnyValue -> Maybe a) -> Value t -> Maybe [a]
matchArray matchValue (Array a) = mapM (liftMatch matchValue) a
matchArray _            _       = Nothing

liftMatch :: (AnyValue -> Maybe a) -> (Value t -> Maybe a)
liftMatch fromAnyValue = fromAnyValue . AnyValue

-- | Checks whether all elements inside given list of 'AnyValue' have the same
-- type as given 'Value'. Returns list of @Value t@ without given 'Value'.
reifyAnyValues :: Value t -> [AnyValue] -> Either TypeMismatchError [Value t]
reifyAnyValues _ []                 = Right []
reifyAnyValues v (AnyValue av : xs) = sameValue v av >>= \Refl -> (av :) <$> reifyAnyValues v xs
