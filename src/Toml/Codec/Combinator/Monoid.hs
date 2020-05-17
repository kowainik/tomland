{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

TOML-specific combinators for converting between TOML and Haskell 'Monoid'
wrapper data types. These codecs are especially handy when you are implementing
the [Partial Options Monoid](https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67)
pattern.

+-----------------------+------------+----------------------------+---------------------+
|     Haskell Type      |   @TOML@   |        'TomlCodec'         | Default on          |
|                       |            |                            | missing field       |
+=======================+============+============================+=====================+
| __'All'__             | @a = true@ | @'all' "a"@                | @'All' 'True'@      |
+-----------------------+------------+----------------------------+---------------------+
| __'Any'__             | @a = true@ | @'any' "a"@                | @'Any' 'False'@     |
+-----------------------+------------+----------------------------+---------------------+
| __@'Sum' 'Int'@__     | @a = 11@   | @'sum' 'Toml.int' "a"@     | @'Sum' 0@           |
+-----------------------+------------+----------------------------+---------------------+
| __@'Product' 'Int'@__ | @a = 11@   | @'product' 'Toml.int' "a"@ | @'Product' 1@       |
+-----------------------+------------+----------------------------+---------------------+
| __@'First' 'Int'@__   | @a = 42@   | @'first' 'Toml.int' "a"@   | @'First' 'Nothing'@ |
+-----------------------+------------+----------------------------+---------------------+
| __@'Last' 'Bool'@__   | @a = true@ | @'last' 'Toml.bool' "a"@   | @'Last' 'Nothing'@  |
+-----------------------+------------+----------------------------+---------------------+

@since 1.3.0.0
-}

module Toml.Codec.Combinator.Monoid
    ( -- * Codecs for 'Monoid's
      -- ** Bool wrappers
      all
    , any
      -- ** 'Num' wrappers
    , sum
    , product
      -- ** 'Maybe' wrappers
    , first
    , last
    ) where

import Prelude hiding (all, any, last, product, sum)

import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))

import Toml.Codec.Combinator.Primitive (bool)
import Toml.Codec.Di (dimap, dioptional, diwrap)
import Toml.Codec.Types (TomlCodec)
import Toml.Type.Key (Key)


{- | Codec for 'All' wrapper for boolean values.
Returns @'All' 'True'@ on missing fields.

Decodes to @'All' 'True'@ when the key is not present.

@since 1.2.1.0
-}
all :: Key -> TomlCodec All
all = dimap (Just . getAll) (maybe mempty All) . dioptional . bool
{-# INLINE all #-}

{- | Codec for 'Any' wrapper for boolean values.
Returns @'Any' 'False'@ on missing fields.

Decodes to @'Any' 'False'@ when the key is not present.

@since 1.2.1.0
-}
any :: Key -> TomlCodec Any
any = dimap (Just . getAny) (maybe mempty Any) . dioptional . bool
{-# INLINE any #-}

{- | Codec for 'Sum' wrapper for given converter's values.

Decodes to @'Sum' 0@ when the key is not present.

@since 1.2.1.0
-}
sum :: (Num a) => (Key -> TomlCodec a) -> Key -> TomlCodec (Sum a)
sum codec = dimap (Just . getSum) (maybe mempty Sum) . dioptional . codec
{-# INLINE sum #-}

{- | Codec for 'Product' wrapper for given converter's values.

Decodes to @'Product' 1@ when the key is not present.

@since 1.2.1.0
-}
product :: (Num a) => (Key -> TomlCodec a) -> Key -> TomlCodec (Product a)
product codec = dimap (Just . getProduct) (maybe mempty Product) . dioptional . codec
{-# INLINE product #-}

{- | Codec for 'First' wrapper for given converter's values.

Decodes to @'First' 'Nothing'@ when the key is not present.

@since 1.2.1.0
-}
first :: (Key -> TomlCodec a) -> Key -> TomlCodec (First a)
first codec = diwrap . dioptional . codec
{-# INLINE first #-}

{- | Codec for 'Last' wrapper for given converter's values.

Decodes to @'Last' 'Nothing'@ when the key is not present.

@since 1.2.1.0
-}
last :: (Key -> TomlCodec a) -> Key -> TomlCodec (Last a)
last codec = diwrap . dioptional . codec
{-# INLINE last #-}
