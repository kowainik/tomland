{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

module Toml.Type.AnyValue
       ( AnyValue (..)
       , MatchError (..)
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

       -- * Util
       , matchError
       ) where

import Control.DeepSeq (NFData, rnf)
import Data.Text (Text)
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (Proxy (..), Typeable, typeRep)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

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

-- Value mismatch error
data MatchError = MatchError
    { valueExpected :: TValue
    , valueActual   :: AnyValue
    } deriving (Eq, Show, Generic, NFData)

----------------------------------------------------------------------------
-- Matching functions for values
----------------------------------------------------------------------------

-- | Extract 'Bool' from 'Value'.
matchBool :: Value t -> Either MatchError Bool
matchBool (Bool b) = Right b
matchBool value    = matchError value

-- | Extract 'Integer' from 'Value'.
matchInteger :: Value t -> Either MatchError Integer
matchInteger (Integer n) = Right n
matchInteger value       = matchError value

-- | Extract 'Double' from 'Value'.
matchDouble :: Value t -> Either MatchError Double
matchDouble (Double f) = Right f
matchDouble value      = matchError value

-- | Extract 'Text' from 'Value'.
matchText :: Value t -> Either MatchError Text
matchText (Text s) = Right s
matchText value    = matchError value

-- | Extract 'DateTime' from 'Value'.
matchDate :: Value t -> Either MatchError DateTime
matchDate (Date d) = Right d
matchDate value    = matchError value

-- | Extract list of elements of type @a@ from array.
matchArray :: (AnyValue -> Either MatchError a) -> Value t -> Either MatchError [a]
matchArray matchValue (Array a) = mapM (liftMatch matchValue) a
matchArray _          value     = matchError value

liftMatch :: (AnyValue -> Either MatchError a) -> (Value t -> Either MatchError a)
liftMatch fromAnyValue = fromAnyValue . AnyValue

-- | Checks whether all elements inside given list of 'AnyValue' have the same
-- type as given 'Value'. Returns list of @Value t@ without given 'Value'.
reifyAnyValues :: Value t -> [AnyValue] -> Either TypeMismatchError [Value t]
reifyAnyValues _ []                 = Right []
reifyAnyValues v (AnyValue av : xs) = sameValue v av >>= \Refl -> (av :) <$> reifyAnyValues v xs

-- | Function for creating 'Array' from list of 'AnyValue'.
toMArray :: [AnyValue] -> Either MatchError (Value 'TArray)
toMArray [] = Right $ Array []
toMArray (AnyValue x : xs) = case reifyAnyValues x xs of
    Left _     -> Left $ MatchError TArray (AnyValue x)
    Right vals -> Right $ Array (x : vals)

----------------------------------------------------------------------
-- Useful functions
----------------------------------------------------------------------

-- | Expacted value
typeName :: forall a . Typeable a => TValue
typeName = case readMaybe . show . typeRep $ Proxy @a of
    Nothing -> error "unknown TValue"
    Just value -> value

-- | Left error part of MatchError.
matchError :: forall (t :: TValue) b . Value t -> Either MatchError b
matchError = Left . MatchError (typeName @TValue) . AnyValue
