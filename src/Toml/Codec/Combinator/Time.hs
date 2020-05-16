{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains TOML-specific combinators for converting between TOML and user data
types.

@since 1.3.0.0
-}

module Toml.Codec.Combinator.Time
    ( zonedTime
    , localTime
    , day
    , timeOfDay
    ) where

import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)

import Toml.Codec.BiMap.Conversion (_Day, _LocalTime, _TimeOfDay, _ZonedTime)
import Toml.Codec.Combinator.Common (match)
import Toml.Codec.Types (TomlCodec)
import Toml.Type.Key (Key)


-- | Codec for zoned time values.
zonedTime :: Key -> TomlCodec ZonedTime
zonedTime = match _ZonedTime
{-# INLINE zonedTime #-}

-- | Codec for local time values.
localTime :: Key -> TomlCodec LocalTime
localTime = match _LocalTime
{-# INLINE localTime #-}

-- | Codec for day values.
day :: Key -> TomlCodec Day
day = match _Day
{-# INLINE day #-}

-- | Codec for time of day values.
timeOfDay :: Key -> TomlCodec TimeOfDay
timeOfDay = match _TimeOfDay
{-# INLINE timeOfDay #-}
