{-# LANGUAGE GADTs #-}

module Toml.Type.UValue
       ( UValue (..)
       , typeCheck
       ) where

import Data.Text (Text)
import Data.Type.Equality ((:~:) (..))

import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Value (DateTime, TypeMismatchError, Value (..), sameValue)

-- | Untyped value of 'TOML'. You shouldn't use this type in your code. Use
-- 'Value' instead.
data UValue
    = UBool !Bool
    | UInteger !Integer
    | UDouble !Double
    | UText !Text
    | UDate !DateTime
    | UArray ![UValue]
    deriving (Eq, Show)

-- | Ensures that 'UValue's represents type-safe version of @toml@.
typeCheck :: UValue -> Either TypeMismatchError AnyValue
typeCheck (UBool b)    = rightAny $ Bool b
typeCheck (UInteger n) = rightAny $ Integer n
typeCheck (UDouble f)  = rightAny $ Double f
typeCheck (UText s)    = rightAny $ Text s
typeCheck (UDate d)    = rightAny $ Date d
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
