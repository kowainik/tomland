{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

{- |
Module                  : Toml.Type.AnyValue
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Existential wrapper over 'Value' type and matching functions.

@since 0.0.0
-}

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


{- | Existential wrapper for 'Value'.

@since 0.0.0
-}
data AnyValue = forall (t :: TValue) . AnyValue (Value t)

instance Show AnyValue where
    show (AnyValue v) = show v

instance Eq AnyValue where
    (AnyValue val1) == (AnyValue val2) = case sameValue val1 val2 of
        Right Refl -> val1 == val2
        Left _     -> False

instance NFData AnyValue where
    rnf (AnyValue val) = rnf val

-- | Value type mismatch error.
data MatchError = MatchError
    { valueExpected :: !TValue
    , valueActual   :: !AnyValue
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (NFData)

-- | Helper function to create 'MatchError'.
mkMatchError :: TValue -> Value t -> Either MatchError a
mkMatchError t = Left . MatchError t . AnyValue

----------------------------------------------------------------------------
-- Matching functions for values
----------------------------------------------------------------------------

-- | Extract 'Prelude.Bool' from 'Value'.
matchBool :: Value t -> Either MatchError Bool
matchBool (Bool b) = Right b
matchBool value    = mkMatchError TBool value
{-# INLINE matchBool #-}

-- | Extract 'Prelude.Integer' from 'Value'.
matchInteger :: Value t -> Either MatchError Integer
matchInteger (Integer n) = Right n
matchInteger value       = mkMatchError TInteger value
{-# INLINE matchInteger #-}

-- | Extract 'Prelude.Double' from 'Value'.
matchDouble :: Value t -> Either MatchError Double
matchDouble (Double f) = Right f
matchDouble value      = mkMatchError TDouble value
{-# INLINE matchDouble #-}

-- | Extract 'Data.Text.Text' from 'Value'.
matchText :: Value t -> Either MatchError Text
matchText (Text s) = Right s
matchText value    = mkMatchError TText value
{-# INLINE matchText #-}

-- | Extract 'Data.Time.ZonedTime' from 'Value'.
matchZoned :: Value t -> Either MatchError ZonedTime
matchZoned (Zoned d) = Right d
matchZoned value     = mkMatchError TZoned value
{-# INLINE matchZoned #-}

-- | Extract 'Data.Time.LocalTime' from 'Value'.
matchLocal :: Value t -> Either MatchError LocalTime
matchLocal (Local d) = Right d
matchLocal value     = mkMatchError TLocal value
{-# INLINE matchLocal #-}

-- | Extract 'Data.Time.Day' from 'Value'.
matchDay :: Value t -> Either MatchError Day
matchDay (Day d) = Right d
matchDay value   = mkMatchError TDay value
{-# INLINE matchDay #-}

-- | Extract 'Data.Time.TimeOfDay' from 'Value'.
matchHours :: Value t -> Either MatchError TimeOfDay
matchHours (Hours d) = Right d
matchHours value     = mkMatchError THours value
{-# INLINE matchHours #-}

-- | Extract list of elements of type @a@ from array.
matchArray :: (AnyValue -> Either MatchError a) -> Value t -> Either MatchError [a]
matchArray matchValue (Array a) = mapM (applyAsToAny matchValue) a
matchArray _          value     = mkMatchError TArray value
{-# INLINE matchArray #-}

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
