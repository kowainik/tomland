{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

module Toml.Type.AnyValue
       ( AnyValue (..)
       , reifyAnyValues
       , toMArray

         -- * Matching
       , MatchError (..)
       , mkMatchError
       , matchBool
       , matchInteger
       , matchDouble
       , matchText
       , matchZoned
       , matchLocal
       , matchDay
       , matchHours
       , matchArray
       , applyAsToAny
       ) where

import Control.DeepSeq (NFData, rnf)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Type.Equality ((:~:) (..))
import GHC.Generics (Generic)

import Toml.Type.Value (TValue (..), TypeMismatchError (..), Value (..), sameValue)


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

-- | Helper function to create 'MatchError'.
mkMatchError :: TValue -> Value t -> Either MatchError a
mkMatchError t = Left . MatchError t . AnyValue

----------------------------------------------------------------------------
-- Matching functions for values
----------------------------------------------------------------------------

-- | Extract 'Bool' from 'Value'.
matchBool :: Value t -> Either MatchError Bool
matchBool (Bool b) = Right b
matchBool value    = mkMatchError TBool value

-- | Extract 'Integer' from 'Value'.
matchInteger :: Value t -> Either MatchError Integer
matchInteger (Integer n) = Right n
matchInteger value       = mkMatchError TInteger value

-- | Extract 'Double' from 'Value'.
matchDouble :: Value t -> Either MatchError Double
matchDouble (Double f) = Right f
matchDouble value      = mkMatchError TDouble value

-- | Extract 'Text' from 'Value'.
matchText :: Value t -> Either MatchError Text
matchText (Text s) = Right s
matchText value    = mkMatchError TText value

-- | Extract 'ZonedTime' from 'Value'.
matchZoned :: Value t -> Either MatchError ZonedTime
matchZoned (Zoned d) = Right d
matchZoned value     = mkMatchError TZoned value

-- | Extract 'LocalTime' from 'Value'.
matchLocal :: Value t -> Either MatchError LocalTime
matchLocal (Local d) = Right d
matchLocal value     = mkMatchError TLocal value

-- | Extract 'Day' from 'Value'.
matchDay :: Value t -> Either MatchError Day
matchDay (Day d) = Right d
matchDay value   = mkMatchError TDay value

-- | Extract 'TimeOfDay' from 'Value'.
matchHours :: Value t -> Either MatchError TimeOfDay
matchHours (Hours d) = Right d
matchHours value     = mkMatchError THours value

-- | Extract list of elements of type @a@ from array.
matchArray :: (AnyValue -> Either MatchError a) -> Value t -> Either MatchError [a]
matchArray matchValue (Array a) = mapM (applyAsToAny matchValue) a
matchArray _          value     = mkMatchError TArray value

-- | Make function that works with 'AnyValue' also work with specific 'Value'.
applyAsToAny :: (AnyValue -> r) -> (Value t -> r)
applyAsToAny f = f . AnyValue

-- | Checks whether all elements inside given list of 'AnyValue' have the same
-- type as given 'Value'. Returns list of @Value t@ without given 'Value'.
reifyAnyValues :: Value t -> [AnyValue] -> Either TypeMismatchError [Value t]
reifyAnyValues _ []                 = Right []
reifyAnyValues v (AnyValue av : xs) = sameValue v av >>= \Refl -> (av :) <$> reifyAnyValues v xs

-- | Function for creating 'Array' from list of 'AnyValue'.
toMArray :: [AnyValue] -> Either MatchError (Value 'TArray)
toMArray [] = Right $ Array []
toMArray (AnyValue x : xs) = case reifyAnyValues x xs of
    Left TypeMismatchError{..} -> mkMatchError typeExpected x
    Right vals                 -> Right $ Array (x : vals)
