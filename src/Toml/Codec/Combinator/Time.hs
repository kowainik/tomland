{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

TOML-specific combinators for converting between TOML and Haskell date and time
data types. TOML specification describes date and time primitives you
can use in your configuration. @tomland@ provides mapping of those
primitives to types from the @time@ library.

+-----------------+----------------------------+-------------------+
|  Haskell Type   |           @TOML@           |    'TomlCodec'    |
+=================+============================+===================+
| __'ZonedTime'__ | @a = 2020-05-16T04:32:00Z@ | @'zonedTime' "a"@ |
+-----------------+----------------------------+-------------------+
| __'LocalTime'__ | @a = 2020-05-16T04:32:00@  | @'localTime' "a"@ |
+-----------------+----------------------------+-------------------+
| __'Day'__       | @a = 2020-05-16@           | @'day' "a"@       |
+-----------------+----------------------------+-------------------+
| __'TimeOfDay'__ | @a = 04:32:00@             | @'timeOfDay' "a"@ |
+-----------------+----------------------------+-------------------+

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
