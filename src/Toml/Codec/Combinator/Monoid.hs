{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains TOML-specific combinators for converting between TOML and user data
types.

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
import Toml.Codec.Di (dimap, dioptional)
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
sum codec = dimap getSum Sum . codec
{-# INLINE sum #-}

{- | Codec for 'Product' wrapper for given converter's values.

@since 1.2.1.0
-}
product :: (Key -> TomlCodec a) -> Key -> TomlCodec (Product a)
product codec = dimap getProduct Product . codec
{-# INLINE product #-}

{- | Codec for 'First' wrapper for given converter's values.

@since 1.2.1.0
-}
first :: (Key -> TomlCodec a) -> Key -> TomlCodec (First a)
first codec = dimap getFirst First . dioptional . codec
{-# INLINE first #-}

{- | Codec for 'Last' wrapper for given converter's values.

@since 1.2.1.0
-}
last :: (Key -> TomlCodec a) -> Key -> TomlCodec (Last a)
last codec = dimap getLast Last . dioptional . codec
{-# INLINE last #-}
