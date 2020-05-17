{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains TOML-specific combinators for converting between TOML and user data
types.
-}

module Toml.Codec.Combinator
    ( -- * Basic codecs for primitive values
      -- ** Boolean
      bool
      -- ** Integral numbers
    , integer
    , natural
    , int
    , word
    , word8
      -- ** Floating point numbers
    , double
    , float
      -- ** Text types
    , text
    , lazyText
    , byteString
    , lazyByteString
    , byteStringArray
    , lazyByteStringArray
    , string
      -- ** Time types
    , zonedTime
    , localTime
    , day
    , timeOfDay

      -- * Codecs for containers of primitives
      -- ** Tuples
    , pair
    , triple
      -- ** Arrays
    , arrayOf
    , arraySetOf
    , arrayIntSet
    , arrayHashSetOf
    , arrayNonEmptyOf

      -- * Codecs for 'Monoid's
      -- ** Bool wrappers
    , all
    , any
      -- ** 'Num' wrappers
    , sum
    , product
      -- ** 'Maybe' wrappers
    , first
    , last

      -- * Additional codecs for custom types
      -- $custom
    , textBy
    , read
    , enumBounded
      -- ** Validation
    , validate
    , validateIf

      -- * Combinators for tables
    , table
    , nonEmpty
    , list
    , set
    , hashSet

      -- * Combinators for 'Map's
    , map
    , tableMap

      -- * General construction of codecs
    , match
    ) where

import Prelude hiding (all, any, last, map, product, read, sum)

import Toml.Codec.Combinator.Common (match)
import Toml.Codec.Combinator.Custom (enumBounded, read, textBy, validate, validateIf)
import Toml.Codec.Combinator.List (arrayNonEmptyOf, arrayOf, list, nonEmpty)
import Toml.Codec.Combinator.Map (map, tableMap)
import Toml.Codec.Combinator.Monoid (all, any, first, last, product, sum)
import Toml.Codec.Combinator.Primitive (bool, byteString, byteStringArray, double, float, int,
                                        integer, lazyByteString, lazyByteStringArray, lazyText,
                                        natural, string, text, word, word8)
import Toml.Codec.Combinator.Set (arrayHashSetOf, arrayIntSet, arraySetOf, hashSet, set)
import Toml.Codec.Combinator.Table (table)
import Toml.Codec.Combinator.Time (day, localTime, timeOfDay, zonedTime)
import Toml.Codec.Combinator.Tuple (pair, triple)

{- $custom
This module provides additional combinators that could help in the situation
when some additional manipulations for the standard combinators is required.

/For example,/ 'validate' allows to perform some custom validation on the codec
before encoding. And 'enumBounded' is an automatical codec that uses 'Enum' and
'Bounded' instances of the data type only and provides descriptive error
messages at the same time.
-}
