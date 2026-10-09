{-# LANGUAGE GADTs #-}

{- |
Module                  : Toml.Type.UValue
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Intermediate untype value representation used for parsing.

@since 0.0.0
-}

module Toml.Type.UValue
       ( UValue (..)
       ) where

import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime, zonedTimeToUTC)

import Toml.Type.Key (Key)


{- | Untyped value of @TOML@. You shouldn't use this type in your
code. Use 'Value' instead.

@since 0.0.0
-}
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
    | UTable ![(Key, UValue)]
      -- ^ Inline table: list of @key = value@ pairs, not yet validated.
      --
      -- @since 1.4.0.0
    deriving stock (Show)

-- | @since 0.0.0
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
    (UTable t1)   == (UTable t2)   = t1 == t2
    _             == _             = False
