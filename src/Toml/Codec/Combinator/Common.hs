{-# LANGUAGE FlexibleContexts #-}

{- |
Module                  : Toml.Codec.Combinator.Common
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module implements common utilities for writing custom codecs
without diving into internal implementation details. Most of the time
you don't need to implement your own codecs and can reuse existing
ones. But if you need something that library doesn't provide, you can
find functions in this module useful.

@since 1.3.0.0
-}

module Toml.Codec.Combinator.Common
    ( match
    , whenLeftBiMapError
    ) where

import Control.Monad.State (modify)
import Validation (Validation (..))

import Toml.Codec.BiMap (BiMap (..), TomlBiMap, TomlBiMapError)
import Toml.Codec.Error (TomlDecodeError (..))
import Toml.Codec.Types (Codec (..), TomlCodec, TomlEnv, TomlState, eitherToTomlState)
import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Key (Key)
import Toml.Type.TOML (Entry (..), TOML, insertKeyAnyVal, lookupEntry)
import Toml.Type.Value (Value (..))

import qualified Data.List.NonEmpty as NE


{- | General function to create bidirectional converters for key-value pairs. In
order to use this function you need to create 'TomlBiMap' for your type and
'AnyValue':

@
_MyType :: 'TomlBiMap' MyType 'AnyValue'
@

And then you can create codec for your type using 'match' function:

@
myType :: 'Key' -> 'TomlCodec' MyType
myType = 'match' _MyType
@

A 'Toml.Type.Value.Table' value is stored as an inline table. Reading returns
a table of any kind as a 'Toml.Type.Value.Table' value and an array of tables
defined with @[[key]]@ headers as an array of such values, so e.g.
@'match' ('Toml.Codec.BiMap.Conversion._Array' ('Toml.Codec.BiMap.Conversion._Table' c))@
reads both @key = [ {..} ]@ and @[[key]]@ tables.

@since 0.4.0
-}
match :: forall a . TomlBiMap a AnyValue -> Key -> TomlCodec a
match BiMap{..} key = Codec input output
  where
    input :: TomlEnv a
    input = \toml -> case lookupAnyValue key toml of
        Nothing     -> Failure [KeyNotFound key]
        Just anyVal -> whenLeftBiMapError key (backward anyVal) pure

    output :: a -> TomlState a
    output a = do
        anyVal <- eitherToTomlState $ forward a
        a <$ modify (insertKeyAnyVal key anyVal)

{- | Looks up a value by key: a key/value pair, or a table (as a
'Toml.Type.Value.Table' value), or an array of tables (as an
'Toml.Type.Value.Array' of 'Toml.Type.Value.Table' values).
-}
lookupAnyValue :: Key -> TOML -> Maybe AnyValue
lookupAnyValue key toml = lookupEntry key toml >>= \case
    EValue v       -> Just v
    ETable _ t     -> Just $ AnyValue $ Table t
    ETableArray ts -> Just $ AnyValue $ Array $ map (AnyValue . Table) (NE.toList ts)

{- | Throw error on 'Left', or perform a given action with 'Right'.

@since 1.3.0.0
-}
whenLeftBiMapError
    :: Key
    -> Either TomlBiMapError a
    -> (a -> Validation [TomlDecodeError] b)
    -> Validation [TomlDecodeError] b
whenLeftBiMapError key val action = case val of
    Right a  -> action a
    Left err -> Failure [BiMapError key err]
