{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

TOML-specific combinators for converting between TOML and Haskell Set-like data
types.

+------------------------+----------------------------------+-------------------------------------+
|      Haskell Type      |              @TOML@              |             'TomlCodec'             |
+========================+==================================+=====================================+
| __@'Set' 'Text'@__     | @a = ["foo", "bar", "baz"]@      | @'arraySetOf' 'Toml._Text' "a"@     |
+------------------------+----------------------------------+-------------------------------------+
| __'IntSet'__           | @a = [11, 42]@                   | @'arrayIntSet' "a"@                 |
+------------------------+----------------------------------+-------------------------------------+
| __@'HashSet' 'Text'@__ | @a = ["foo", "bar"]@             | @'arrayHashSetOf' 'Toml._Text' "a"@ |
+------------------------+----------------------------------+-------------------------------------+
| __@'Set' 'Text'@__     | @x = [{a = "foo"}, {a = "bar"}]@ | @'set' ('Toml.text' "a") "x"@       |
+------------------------+----------------------------------+-------------------------------------+
| __@'HashSet' 'Text'@__ | @x = [{a = "foo"}, {a = "bar"}]@ | @'hashSet' ('Toml.text' "a") "x"@   |
+------------------------+----------------------------------+-------------------------------------+

@since 1.3.0.0
-}

module Toml.Codec.Combinator.Set
    ( arraySetOf
    , arrayIntSet
    , arrayHashSetOf
      -- * Table Sets
    , set
    , hashSet
    ) where

import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.IntSet (IntSet)
import Data.Set (Set)

import Toml.Codec.BiMap (TomlBiMap)
import Toml.Codec.BiMap.Conversion (_HashSet, _IntSet, _Set)
import Toml.Codec.Combinator.Common (match)
import Toml.Codec.Combinator.List (list)
import Toml.Codec.Di (dimap)
import Toml.Codec.Types (TomlCodec)
import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Key (Key)

import qualified Data.HashSet as HS
import qualified Data.Set as S


-- | Codec for sets. Takes converter for single value and
-- returns a set of values.
arraySetOf :: Ord a => TomlBiMap a AnyValue -> Key -> TomlCodec (Set a)
arraySetOf = match . _Set
{-# INLINE arraySetOf #-}

-- | Codec for sets of ints. Takes converter for single value and
-- returns a set of ints.
arrayIntSet :: Key -> TomlCodec IntSet
arrayIntSet = match _IntSet
{-# INLINE arrayIntSet #-}

-- | Codec for hash sets. Takes converter for single hashable value and
-- returns a set of hashable values.
arrayHashSetOf
    :: (Hashable a, Eq a)
    => TomlBiMap a AnyValue
    -> Key
    -> TomlCodec (HashSet a)
arrayHashSetOf = match . _HashSet
{-# INLINE arrayHashSetOf #-}

----------------------------------------------------------------------------
-- Tables and arrays of tables
----------------------------------------------------------------------------
{- | 'Codec' for set of values. Represented in TOML as array of tables.

@since 1.2.0.0
-}
set :: forall a . Ord a => TomlCodec a -> Key -> TomlCodec (Set a)
set codec key = dimap S.toList S.fromList (list codec key)
{-# INLINE set #-}

{- | 'Codec' for HashSet of values. Represented in TOML as array of tables.

@since 1.2.0.0
-}

hashSet :: forall a . (Hashable a, Eq a) => TomlCodec a -> Key -> TomlCodec (HashSet a)
hashSet codec key = dimap HS.toList HS.fromList (list codec key)
{-# INLINE hashSet #-}
