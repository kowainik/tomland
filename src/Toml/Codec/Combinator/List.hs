{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

TOML-specific combinators for converting between TOML and Haskell list-like data
types.

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
    ( arrayOf
    , arrayNonEmptyOf

      -- * Table lists
    , list
    , nonEmpty
    ) where

import Control.Monad (forM)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.List.NonEmpty (NonEmpty (..), toList)

import Toml.Codec.BiMap (TomlBiMap)
import Toml.Codec.BiMap.Conversion (_Array, _NonEmpty)
import Toml.Codec.Code (execTomlCodec)
import Toml.Codec.Combinator.Common (codecReadTOML, match)
import Toml.Codec.Combinator.Table (handleErrorInTable)
import Toml.Codec.Error (TomlDecodeError (..))
import Toml.Codec.Types (Codec (..), TomlCodec, TomlEnv, TomlState)
import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Key (Key)
import Toml.Type.TOML (TOML (..), insertTableArrays)

import qualified Data.HashMap.Strict as HashMap


-- | Codec for list of values. Takes converter for single value and
-- returns a list of values.
arrayOf :: TomlBiMap a AnyValue -> Key -> TomlCodec [a]
arrayOf = match . _Array
{-# INLINE arrayOf #-}

-- | Codec for non- empty lists of values. Takes converter for single value and
-- returns a non-empty list of values.
arrayNonEmptyOf :: TomlBiMap a AnyValue -> Key -> TomlCodec (NonEmpty a)
arrayNonEmptyOf = match . _NonEmpty
{-# INLINE arrayNonEmptyOf #-}

-- | 'Codec' for list of values. Represented in TOML as array of tables.
list :: forall a . TomlCodec a -> Key -> TomlCodec [a]
list codec key = Codec
    { codecRead = (toList <$> codecRead nonEmptyCodec) `catchError` \case
        TableNotFound errKey | errKey == key -> pure []
        err -> throwError err
    , codecWrite = \case
        [] -> pure []
        l@(x:xs) -> l <$ codecWrite nonEmptyCodec (x :| xs)
    }
  where
    nonEmptyCodec :: TomlCodec (NonEmpty a)
    nonEmptyCodec = nonEmpty codec key


{- | 'Codec' for 'NonEmpty' list of values. Represented in TOML as array of
tables.
-}
nonEmpty :: forall a . TomlCodec a -> Key -> TomlCodec (NonEmpty a)
nonEmpty codec key = Codec input output
  where
    input :: TomlEnv (NonEmpty a)
    input = do
        mTables <- asks $ HashMap.lookup key . tomlTableArrays
        case mTables of
            Nothing    -> throwError $ TableNotFound key
            Just tomls -> forM tomls $ \toml ->
                codecReadTOML toml codec `catchError` handleErrorInTable key

    -- adds all TOML objects to the existing list if there are some
    output :: NonEmpty a -> TomlState (NonEmpty a)
    output as = do
        let tomls = fmap (execTomlCodec codec) as
        mTables <- gets $ HashMap.lookup key . tomlTableArrays

        let newTomls = case mTables of
                Nothing       -> tomls
                Just oldTomls -> oldTomls <> tomls

        as <$ modify (insertTableArrays key newTomls)
