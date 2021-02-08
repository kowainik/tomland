{- |
Module                  : Toml.Codec.Combinator.Set
Copyright               : (c) 2018-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

TOML-specific combinators for converting between TOML and Haskell Set-like data
types.

There are two way to represent list-like structures with the @tomland@ library.

* Ordinary array sets of primitives:

    @
    foo = [100, 200, 300]
    @

* Sets via tables:

    @
    foo =
        [ {x = 100}
        , {x = 200}
        , {x = 300}
        ]

    __OR__

    [[foo]]
        x = 100
    [[foo]]
        x = 200
    [[foo]]
        x = 300
    @

You can find both types of the codecs in this module for different set-like
structures. See the following table for the better understanding:

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
    ( -- * Array sets
      arraySetOf
    , arrayIntSet
    , arrayHashSetOf
      -- * Table Sets
    , set
    , intSet
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
import qualified Data.IntSet as IS
import qualified Data.Set as S


{- | Codec for sets. Takes converter for single value and
returns a set of values.

__Example:__

Haskell @'Set' 'Int'@ can look like this in your @TOML@ file:

@
foo = [1, 2, 3]
@

In case of the missing field, the following error will be seen:

@
tomland decode error:  Key foo is not found
@

@since 0.5.0
-}
arraySetOf :: Ord a => TomlBiMap a AnyValue -> Key -> TomlCodec (Set a)
arraySetOf = match . _Set
{-# INLINE arraySetOf #-}

{- | Codec for sets of ints. Takes converter for single value and
returns a set of ints.

__Example:__

Haskell @'IntSet'@ can look like this in your @TOML@ file:

@
foo = [1, 2, 3]
@

In case of the missing field, the following error will be seen:

@
tomland decode error:  Key foo is not found
@

@since 0.5.0
-}
arrayIntSet :: Key -> TomlCodec IntSet
arrayIntSet = match _IntSet
{-# INLINE arrayIntSet #-}

{- | Codec for hash sets. Takes converter for single hashable value and
returns a set of hashable values.

__Example:__

Haskell @'HashSet' 'Int'@ can look like this in your @TOML@ file:

@
foo = [1, 2, 3]
@

In case of the missing field, the following error will be seen:

@
tomland decode error:  Key foo is not found
@

@since 0.5.0
-}
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

__Example:__

Haskell @'Set' 'Int'@ can look like this in your @TOML@ file:

@
foo =
  [ {a = 1}
  , {a = 2}
  ]
@

Decodes to an empty 'Set' in case of the missing field in @TOML@.

@since 1.2.0.0
-}
set :: forall a . Ord a => TomlCodec a -> Key -> TomlCodec (Set a)
set codec key = dimap S.toList S.fromList (list codec key)
{-# INLINE set #-}

{- | 'Codec' for 'IntSet'. Represented in TOML as an array of tables.

__Example:__

Haskell 'IntSet' can look like this in your @TOML@ file:

@
foo =
  [ {a = 1}
  , {a = 2}
  ]
@

Decodes to an empty 'IntSet' in case of the missing field in @TOML@.

@since 1.3.0.0
-}
intSet :: TomlCodec Int -> Key -> TomlCodec IntSet
intSet codec key = dimap IS.toList IS.fromList (list codec key)
{-# INLINE intSet #-}

{- | 'Codec' for 'HashSet' of values. Represented in TOML as an array of tables.

__Example:__

Haskell @'HashSet' 'Int'@ can look like this in your @TOML@ file:

@
foo =
  [ {a = 1}
  , {a = 2}
  ]
@

Decodes to an empty 'HashSet' in case of the missing field in @TOML@.

@since 1.2.0.0
-}
hashSet :: forall a . (Hashable a, Eq a) => TomlCodec a -> Key -> TomlCodec (HashSet a)
hashSet codec key = dimap HS.toList HS.fromList (list codec key)
{-# INLINE hashSet #-}
