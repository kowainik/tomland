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
    , handleTableErrors
    , mapTableErrors
    ) where

import Control.Monad.State (gets, modify)
import Data.Maybe (fromMaybe)
import Validation (Validation (..))

import Toml.Codec.Error (TomlDecodeError (..))
import Toml.Codec.Types (Codec (..), TomlCodec, TomlEnv, TomlState (..))
import Toml.Type.Key (Key)
import Toml.Type.TOML (TOML (..), insertTable)

import qualified Toml.Type.PrefixTree as Prefix



{- | Maps errors in tables with 'mapTableErrors'

@since 1.3.0.0
-}
handleTableErrors :: TomlCodec a -> Key -> TOML -> Validation [TomlDecodeError] a
handleTableErrors codec key toml = case codecRead codec toml of
    Success res  -> Success res
    Failure errs -> Failure $ mapTableErrors key errs

{- | Prepends given key to all errors that contain key. This function is used to
give better error messages. So when error happens we know all pieces of table
key, not only the last one.

@since 0.2.0
-}
mapTableErrors :: Key -> [TomlDecodeError] -> [TomlDecodeError]
mapTableErrors key = map (\case
    KeyNotFound name        -> KeyNotFound (key <> name)
    TableNotFound name      -> TableNotFound (key <> name)
    TableArrayNotFound name -> TableArrayNotFound (key <> name)
    e                       -> e
    )

{- | Codec for tables. Use it when when you have nested objects.

@since 0.2.0
-}
table :: forall a . TomlCodec a -> Key -> TomlCodec a
table codec key = Codec input output
  where
    input :: TomlEnv a
    input = \t -> case Prefix.lookup key $ tomlTables t of
        Nothing   -> Failure [TableNotFound key]
        Just toml -> handleTableErrors codec key toml

    output :: a -> TomlState a
    output a = do
        mTable <- gets $ Prefix.lookup key . tomlTables
        let toml = fromMaybe mempty mTable
        let (_, newToml) = unTomlState (codecWrite codec a) toml
        a <$ modify (insertTable key newToml)
