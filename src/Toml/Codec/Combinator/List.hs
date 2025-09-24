{- |
Module                  : Toml.Codec.Combinator.List
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

TOML-specific combinators for converting between TOML and Haskell list-like data
types.

There are two way to represent list-like structures with the @tomland@ library.

* Ordinary array lists of primitives:

    @
    foo = [100, 200, 300]
    @

* Lists via tables:

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

You can find both types of the codecs in this module for different list-like
structures. See the following table for the better understanding:

+-------------------------+----------------------------------+-------------------------------------+
|      Haskell Type       |              @TOML@              |             'TomlCodec'             |
+=========================+==================================+=====================================+
| __@['Int']@__           | @a = [1, 2, 3]@                  | @'arrayOf' 'Toml._Int' "a"@         |
+-------------------------+----------------------------------+-------------------------------------+
| __@'NonEmpty' 'Int'@__  | @a = [11, 42]@                   | @'arrayNonEmptyOf' 'Toml._Int' "a"@ |
+-------------------------+----------------------------------+-------------------------------------+
| __@['Text']@__          | @x = [{a = "foo"}, {a = "bar"}]@ | @'list' ('Toml.text' "a") "x"@      |
+-------------------------+----------------------------------+-------------------------------------+
| __@'NonEmpty' 'Text'@__ | @x = [{a = "foo"}, {a = "bar"}]@ | @'nonEmpty' ('Toml.text' "a") "x"@  |
+-------------------------+----------------------------------+-------------------------------------+

@since 1.3.0.0
-}

module Toml.Codec.Combinator.List
    ( -- * Array lists
      arrayOf
    , arrayNonEmptyOf

      -- * Table lists
    , list
    , nonEmpty
    ) where

import Control.Monad.State (gets, modify)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Validation (Validation (..))

import Toml.Codec.BiMap (TomlBiMap)
import Toml.Codec.BiMap.Conversion (_Array, _NonEmpty)
import Toml.Codec.Code (execTomlCodec)
import Toml.Codec.Combinator.Common (match)
import Toml.Codec.Combinator.Table (handleTableErrors)
import Toml.Codec.Error (TomlDecodeError (..))
import Toml.Codec.Types (Codec (..), TomlCodec, TomlEnv, TomlState)
import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Key (Key)
import Toml.Type.TOML (TOML (..), insertTableArrays)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty


{- | Codec for list of values. Takes converter for single value and
returns a list of values.

__Example:__

Haskell @['Int']@ can look like this in your @TOML@ file:

@
foo = [1, 2, 3]
@

If the key is not present in @TOML@ the following decode error will be spotted:

@
tomland decode error:  Key foo is not found
@

@since 0.1.0
-}
arrayOf :: TomlBiMap a AnyValue -> Key -> TomlCodec [a]
arrayOf = match . _Array
{-# INLINE arrayOf #-}

{- | Codec for non- empty lists of values. Takes converter for single value and
returns a non-empty list of values.

__Example:__

Haskell @'NonEmpty' 'Int'@ can look like this in your @TOML@ file:

@
foo = [1, 2, 3]
@

If you try to decode an empty @TOML@ list you will see the error:

@
tomland decode error:  Empty array list, but expected NonEmpty
@

If the key is not present in @TOML@ the following decode error will be spotted:

@
tomland decode error:  Key foo is not found
@

@since 0.5.0
-}
arrayNonEmptyOf :: TomlBiMap a AnyValue -> Key -> TomlCodec (NonEmpty a)
arrayNonEmptyOf = match . _NonEmpty
{-# INLINE arrayNonEmptyOf #-}

{- | 'Codec' for list of values. Represented in TOML as array of tables.

__Example:__

Haskell @['Int']@ can look like this in your @TOML@ file:

@
foo =
  [ {a = 1}
  , {a = 2}
  , {a = 3}
  ]
@

Decodes to an empty list @[]@ when the key is not present.

@since 1.0.0
-}
list :: forall a . TomlCodec a -> Key -> TomlCodec [a]
list codec key = Codec
    { codecRead = \toml -> case codecRead nonEmptyCodec toml of
        Success ne -> Success $ toList ne
        Failure [TableArrayNotFound errKey]
            | errKey == key -> pure []
        Failure errs -> Failure errs
    , codecWrite = traverse_ (codecWrite nonEmptyCodec) . NonEmpty.nonEmpty
    }
  where
    nonEmptyCodec :: TomlCodec (NonEmpty a)
    nonEmptyCodec = nonEmpty codec key


{- | 'Codec' for 'NonEmpty' list of values. Represented in TOML as array of
tables.

__Example:__

Haskell @'NonEmpty' 'Int'@ can look like this in your @TOML@ file:

@
foo =
  [ {a = 1}
  , {a = 2}
  , {a = 3}
  ]
@

If you try to decode an empty @TOML@ list you will see the error:

@
tomland decode error:  Table array [[foo]] is not found
@

or

@
tomland decode error:  Key foo.a is not found
@

If the key is not present in @TOML@ the following decode error will be spotted:

@
tomland decode error:  Table array [[foo]] is not found
@

@since 1.0.0
-}
nonEmpty :: forall a . TomlCodec a -> Key -> TomlCodec (NonEmpty a)
nonEmpty codec key = Codec input output
  where
    input :: TomlEnv (NonEmpty a)
    input = \t -> case HashMap.lookup key $ tomlTableArrays t of
        Nothing    -> Failure [TableArrayNotFound key]
        Just tomls -> traverse (handleTableErrors codec key) tomls


    -- adds all TOML objects to the existing list if there are some
    output :: NonEmpty a -> TomlState ()
    output as = do
        let tomls = fmap (execTomlCodec codec) as
        mTables <- gets $ HashMap.lookup key . tomlTableArrays

        let newTomls = case mTables of
                Nothing       -> tomls
                Just oldTomls -> oldTomls <> tomls

        modify (insertTableArrays key newTomls)
