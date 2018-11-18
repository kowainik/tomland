{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

module Toml.Type.AnyValue
       ( AnyValue (..)
       , BiMapError (..)
       , reifyAnyValues
       , toMArray

         -- * Matching
       , liftMatch
       , matchBool
       , matchInteger
       , matchDouble
       , matchText
       , matchDate
       , matchArray
       ) where

import Control.DeepSeq (NFData, rnf)
import Data.Text (Text)
import Data.Type.Equality ((:~:) (..))

import Toml.Type.Value (DateTime, TValue (..), TypeMismatchError, Value (..), sameValue)

-- | Existential wrapper for 'Value'.
data AnyValue = forall (t :: TValue) . AnyValue (Value t)

instance Show AnyValue where
    show (AnyValue v) = show v

instance Eq AnyValue where
    (AnyValue val1) == (AnyValue val2) = case sameValue val1 val2 of
        Right Refl -> val1 == val2
        Left _     -> False

instance NFData AnyValue where
    rnf (AnyValue val) = rnf val

----------------------------------------------------------------------------
-- Matching functions for values
----------------------------------------------------------------------------

-- | Extract 'Bool' from 'Value'.
matchBool :: Value t -> Either BiMapError Bool
matchBool (Bool b) = Right b
matchBool _        = Left BiMapError

-- | Extract 'Integer' from 'Value'.
matchInteger :: Value t -> Either BiMapError Integer
matchInteger (Integer n) = Right n
matchInteger _           = Left BiMapError

-- | Extract 'Double' from 'Value'.
matchDouble :: Value t -> Either BiMapError Double
matchDouble (Double f) = Right f
matchDouble _          = Left BiMapError

-- | Extract 'Text' from 'Value'.
matchText :: Value t -> Either BiMapError Text
matchText (Text s) = Right s
matchText _        = Left BiMapError

-- | Extract 'DateTime' from 'Value'.
matchDate :: Value t -> Either BiMapError DateTime
matchDate (Date d) = Right d
matchDate _        = Left BiMapError

-- | Extract list of elements of type @a@ from array.
matchArray :: (AnyValue -> Either BiMapError a) -> Value t -> Either BiMapError [a]
matchArray matchValue (Array a) = mapM (liftMatch matchValue) a
matchArray _            _       = Left BiMapError

liftMatch :: (AnyValue -> Either BiMapError a) -> (Value t -> Either BiMapError a)
liftMatch fromAnyValue = fromAnyValue . AnyValue

-- | Checks whether all elements inside given list of 'AnyValue' have the same
-- type as given 'Value'. Returns list of @Value t@ without given 'Value'.
reifyAnyValues :: Value t -> [AnyValue] -> Either TypeMismatchError [Value t]
reifyAnyValues _ []                 = Right []
reifyAnyValues v (AnyValue av : xs) = sameValue v av >>= \Refl -> (av :) <$> reifyAnyValues v xs

-- | Function for creating 'Array' from list of 'AnyValue'.
toMArray :: [AnyValue] -> Either BiMapError (Value 'TArray)
toMArray [] = Right $ Array []
toMArray (AnyValue x : xs) = case reifyAnyValues x xs of
    Left _     -> Left BiMapError
    Right vals -> Right $ Array (x : vals)

----------------------------------------------------------------------------
-- BiMap error type
----------------------------------------------------------------------------

data BiMapError = BiMapError
