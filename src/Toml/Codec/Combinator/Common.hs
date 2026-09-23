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
    , thenValidate
    ) where

import Control.Monad.State (modify)
import Validation (Validation (..))

import Toml.Codec.BiMap (BiMap (..), TomlBiMap, TomlBiMapError)
import Toml.Codec.Error (TomlDecodeError (..))
import Toml.Codec.Types (Codec (..), TomlCodec, TomlEnv, TomlState, eitherToTomlState)
import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Key (Key)
import Toml.Type.TOML (TOML (..), insertKeyAnyVal)

import qualified Data.HashMap.Strict as HashMap


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

@since 0.4.0
-}
match :: forall a . TomlBiMap a AnyValue -> Key -> TomlCodec a
match BiMap{..} key = Codec input output
  where
    input :: TomlEnv a
    input = \toml -> case HashMap.lookup key (tomlPairs toml) of
        Nothing     -> Failure [KeyNotFound key]
        Just anyVal -> whenLeftBiMapError key (backward anyVal) pure

    output :: a -> TomlState a
    output a = do
        anyVal <- eitherToTomlState $ forward a
        a <$ modify (insertKeyAnyVal key anyVal)

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


{- | Perform additional validation after decoding the value with the given codec.

@since 1.3.3.3
-}
thenValidate
    :: (a -> Validation.Validation [TomlDecodeError] a)
    -> TomlCodec a
    -> TomlCodec a
thenValidate f (Codec readC writeC) = Codec readThenValidateC writeC
  where
    readThenValidateC toml = case readC toml of
        Validation.Success success -> f success
        Validation.Failure errors -> Validation.Failure errors

