{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains TOML-specific combinators for converting between TOML and user data
types.

Tables can be represented in @TOML@ in one of the following ways:

@
foo =
    { x = ...
    , y = ...
    , ...
    }
@

__Or__

@
[foo]
    x = ...
    y = ...
    ...
@

@since 1.3.0.0
-}

module Toml.Codec.Combinator.Table
    ( -- * Tables
      table
      -- * Error Helpers
    , handleErrorInTable
    ) where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks)
import Control.Monad.State (execState, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (fromMaybe)

import Toml.Codec.Combinator.Common (codecReadTOML)
import Toml.Codec.Error (TomlDecodeError (..))
import Toml.Codec.Types (Codec (..), TomlCodec, TomlEnv, TomlState)
import Toml.Type.Key (Key)
import Toml.Type.TOML (TOML (..), insertTable)

import qualified Toml.Type.PrefixTree as Prefix


{- | Prepends given key to all errors that contain key. This function is used to
give better error messages. So when error happens we know all pieces of table
key, not only the last one.

@since 0.2.0
-}
handleErrorInTable :: Key -> TomlDecodeError -> TomlEnv a
handleErrorInTable key = \case
    KeyNotFound name        -> throwError $ KeyNotFound (key <> name)
    TableNotFound name      -> throwError $ TableNotFound (key <> name)
    TableArrayNotFound name -> throwError $ TableArrayNotFound (key <> name)
    TypeMismatch name t1 t2 -> throwError $ TypeMismatch (key <> name) t1 t2
    e                       -> throwError e

{- | Codec for tables. Use it when when you have nested objects.

@since 0.2.0
-}
table :: forall a . TomlCodec a -> Key -> TomlCodec a
table codec key = Codec input output
  where
    input :: TomlEnv a
    input = do
        mTable <- asks $ Prefix.lookup key . tomlTables
        case mTable of
            Nothing   -> throwError $ TableNotFound key
            Just toml -> codecReadTOML toml codec `catchError` handleErrorInTable key

    output :: a -> TomlState a
    output a = do
        mTable <- gets $ Prefix.lookup key . tomlTables
        let toml = fromMaybe mempty mTable
        let newToml = execState (runMaybeT $ codecWrite codec a) toml
        a <$ modify (insertTable key newToml)
