{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

TOML-specific combinators for converting between TOML and Haskell 'Monoid'
wrapper data types.

+-----------------------+------------+----------------------------+
|     Haskell Type      |   @TOML@   |        'TomlCodec'         |
+=======================+============+============================+
| __'All'__             | @a = true@ | @'all' "a"@                |
+-----------------------+------------+----------------------------+
| __'Any'__             | @a = true@ | @'any' "a"@                |
+-----------------------+------------+----------------------------+
| __@'Sum' 'Int'@__     | @a = 1@    | @'sum' 'Toml.int' "a"@     |
+-----------------------+------------+----------------------------+
| __@'Product' 'Int'@__ | @a = 0@    | @'product' 'Toml.int' "a"@ |
+-----------------------+------------+----------------------------+
| __@'First' 'Int'@__   | @a = 42@   | @'first' 'Toml.int' "a"@   |
+-----------------------+------------+----------------------------+
| __@'Last' 'Bool'@__   | @a = true@ | @'last' 'Toml.bool' "a"@   |
+-----------------------+------------+----------------------------+

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

import Data.Maybe (fromMaybe)
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))

import Toml.Codec.Combinator.Primitive (bool)
import Toml.Codec.Di (dimap, dioptional, diwrap)
import Toml.Codec.Types (TomlCodec)
import Toml.Type.Key (Key)


{- | Codec for 'All' wrapper for boolean values.
Returns @'All' 'True'@ on missing fields.

@since 1.2.1.0
-}
all :: Key -> TomlCodec All
all = dimap (Just . getAll) (All . fromMaybe True) . dioptional . bool
{-# INLINE all #-}

{- | Codec for 'Any' wrapper for boolean values.
Returns @'Any' 'False'@ on missing fields.

@since 1.2.1.0
-}
any :: Key -> TomlCodec Any
any = dimap (Just . getAny) (Any . fromMaybe False) . dioptional . bool
{-# INLINE any #-}

{- | Codec for 'Sum' wrapper for given converter's values.

@since 1.2.1.0
-}
sum :: (Key -> TomlCodec a) -> Key -> TomlCodec (Sum a)
sum codec = diwrap . codec
{-# INLINE sum #-}

{- | Codec for 'Product' wrapper for given converter's values.

@since 1.2.1.0
-}
product :: (Key -> TomlCodec a) -> Key -> TomlCodec (Product a)
product codec = diwrap . codec
{-# INLINE product #-}

{- | Codec for 'First' wrapper for given converter's values.

@since 1.2.1.0
-}
first :: (Key -> TomlCodec a) -> Key -> TomlCodec (First a)
first codec = diwrap . dioptional . codec
{-# INLINE first #-}

{- | Codec for 'Last' wrapper for given converter's values.

@since 1.2.1.0
-}
last :: (Key -> TomlCodec a) -> Key -> TomlCodec (Last a)
last codec = diwrap . dioptional . codec
{-# INLINE last #-}
