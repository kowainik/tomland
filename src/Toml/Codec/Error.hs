{-# LANGUAGE DeriveAnyClass #-}

{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Core error types, including 'TomlDecodeError' and 'LoadTomlException'.

@since 1.3.0.0
-}

module Toml.Codec.Error
    ( TomlDecodeError (..)
    , prettyTomlDecodeError

    , LoadTomlException (..)
    ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Data.Text (Text)
import GHC.Generics (Generic)

import Toml.Codec.BiMap (TomlBiMapError, prettyBiMapError)
import Toml.Parser (TomlParseError (..))
import Toml.Type.Key (Key (..))
import Toml.Type.Printer (prettyKey)
import Toml.Type.Value (TValue, showType)

import qualified Data.Text as Text


{- | Type of exception for converting from TOML to user custom data type.

@since 1.3.0.0
-}
data TomlDecodeError
    = TrivialError
    | BiMapError !TomlBiMapError
    | KeyNotFound !Key  -- ^ No such key
    | TableNotFound !Key  -- ^ No such table
    | TypeMismatch !Key !Text !TValue  -- ^ Expected type vs actual type
    | ParseError !TomlParseError  -- ^ Exception during parsing
    deriving stock (Eq, Generic)
    deriving anyclass (NFData)

instance Show TomlDecodeError where
    show = Text.unpack . prettyTomlDecodeError

instance Semigroup TomlDecodeError where
    TrivialError <> e = e
    e <> _ = e

instance Monoid TomlDecodeError where
    mempty = TrivialError
    mappend = (<>)

{- | Converts 'TomlDecodeError' into pretty human-readable text.

@since 1.3.0.0
-}
prettyTomlDecodeError :: TomlDecodeError -> Text
prettyTomlDecodeError de = "tomland decode error:  " <> case de of
    TrivialError -> "'empty' parser from 'Alternative' is used"
    BiMapError biError -> prettyBiMapError biError
    KeyNotFound name -> "Key " <> prettyKey name <> " is not found"
    TableNotFound name -> "Table [" <> prettyKey name <> "] is not found"
    TypeMismatch name expected actual -> "Type for key " <> prettyKey name <> " doesn't match."
        <> "\n  Expected: " <> expected
        <> "\n  Actual:   " <> Text.pack (showType actual)
    ParseError (TomlParseError msg) ->
        "Parse error during conversion from TOML to custom user type: \n  " <> msg

{- | File loading error data type.

@since 0.3.1
-}
data LoadTomlException = LoadTomlException !FilePath !Text

-- | @since 0.3.1
instance Show LoadTomlException where
    show (LoadTomlException filePath msg) = "Couldnt parse file " ++ filePath ++ ": " ++ show msg

-- | @since 0.3.1
instance Exception LoadTomlException
