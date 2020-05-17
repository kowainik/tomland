{-# LANGUAGE FlexibleContexts #-}

{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

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
    , codecReadTOML
    ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (modify)
import Control.Monad.Trans.Maybe (MaybeT (..))

import Toml.Codec.BiMap (BiMap (..), TomlBiMap, TomlBiMapError)
import Toml.Codec.Error (TomlDecodeError (..))
import Toml.Codec.Types (Codec (..), TomlCodec, TomlEnv, TomlState)
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
-}
match :: forall a . TomlBiMap a AnyValue -> Key -> TomlCodec a
match BiMap{..} key = Codec input output
  where
    input :: TomlEnv a
    input = asks (HashMap.lookup key . tomlPairs) >>= \case
        Nothing     -> throwError $ KeyNotFound key
        Just anyVal -> whenLeftBiMapError (backward anyVal) pure

    output :: a -> TomlState a
    output a = do
        anyVal <- MaybeT $ pure $ either (const Nothing) Just $ forward a
        a <$ modify (insertKeyAnyVal key anyVal)


-- | Throw error on 'Left', or perform a given action with 'Right'.
whenLeftBiMapError
    :: (MonadError TomlDecodeError m)
    => Either TomlBiMapError a
    -> (a -> m b)
    -> m b
whenLeftBiMapError val action = case val of
    Right a  -> action a
    Left err -> throwError $ BiMapError err

-- | Run 'codecRead' function with given 'TOML' inside 'Control.Monad.Reader.ReaderT' context.
codecReadTOML :: TOML -> TomlCodec a -> TomlEnv a
codecReadTOML toml codec = local (const toml) (codecRead codec)
