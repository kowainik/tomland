{-# LANGUAGE GADTs #-}

-- | Intermediate untype value representation used for parsing.

module Toml.Type.UValue
       ( UValue (..)
       , typeCheck
       ) where

import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime, zonedTimeToUTC)
import Data.Type.Equality ((:~:) (..))

import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Value (TypeMismatchError, Value (..), sameValue)


-- | Untyped value of @TOML@. You shouldn't use this type in your code. Use
-- 'Value' instead.
data UValue
    = UBool !Bool
    | UInteger !Integer
    | UDouble !Double
    | UText !Text
    | UZoned !ZonedTime
    | ULocal !LocalTime
    | UDay !Day
    | UHours !TimeOfDay
    | UArray ![UValue]
    deriving stock (Show)

instance Eq UValue where
    (UBool b1)    == (UBool b2)    = b1 == b2
    (UInteger i1) == (UInteger i2) = i1 == i2
    (UDouble f1)  == (UDouble f2)
        | isNaN f1 && isNaN f2 = True
        | otherwise = f1 == f2
    (UText s1)    == (UText s2)    = s1 == s2
    (UZoned a)    == (UZoned b)    = zonedTimeToUTC a == zonedTimeToUTC b
    (ULocal a)    == (ULocal b)    = a == b
    (UDay a)      == (UDay b)      = a == b
    (UHours a)    == (UHours b)    = a == b
    (UArray a1)   == (UArray a2)   = a1 == a2
    _             == _             = False

-- | Ensures that 'UValue's represents type-safe version of @toml@.
typeCheck :: UValue -> Either TypeMismatchError AnyValue
typeCheck (UBool b)    = rightAny $ Bool b
typeCheck (UInteger n) = rightAny $ Integer n
typeCheck (UDouble f)  = rightAny $ Double f
typeCheck (UText s)    = rightAny $ Text s
typeCheck (UZoned d)   = rightAny $ Zoned d
typeCheck (ULocal d)   = rightAny $ Local d
typeCheck (UDay d)     = rightAny $ Day d
typeCheck (UHours d)   = rightAny $ Hours d
typeCheck (UArray a)   = case a of
    []   -> rightAny $ Array []
    x:xs -> do
        AnyValue v <- typeCheck x
        AnyValue . Array <$> checkElem v xs
  where
    checkElem :: Value t -> [UValue] -> Either TypeMismatchError [Value t]
    checkElem v []     = Right [v]
    checkElem v (x:xs) = do
        AnyValue vx <- typeCheck x
        Refl <- sameValue v vx
        (v :) <$> checkElem vx xs

rightAny :: Value t -> Either l AnyValue
rightAny = Right . AnyValue
