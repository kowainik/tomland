{-# LANGUAGE DeriveAnyClass #-}

{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Core error types, including 'TomlDecodeError' and 'LoadTomlException'.
-}

module Toml.Codec.Error
    ( TomlDecodeError (..)
    , prettyTomlDecodeError

    , LoadTomlException (..)
    ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Data.Foldable (toList)
import Data.Text (Text)
import GHC.Generics (Generic)

import Toml.Bi.Map (TomlBiMapError, prettyBiMapError)
import Toml.Parser (ParseException (..))
import Toml.PrefixTree (Key (..), unPiece)
import Toml.Type (TValue, showType)

import qualified Data.Text as Text


-- | Type of exception for converting from TOML to user custom data type.
data TomlDecodeError
    = TrivialError
    | BiMapError !TomlBiMapError
    | KeyNotFound !Key  -- ^ No such key
    | TableNotFound !Key  -- ^ No such table
    | TypeMismatch !Key !Text !TValue  -- ^ Expected type vs actual type
    | ParseError !ParseException  -- ^ Exception during parsing
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

-- | Converts 'TomlDecodeError' into pretty human-readable text.
prettyTomlDecodeError :: TomlDecodeError -> Text
prettyTomlDecodeError de = "tomland decode error:  " <> case de of
    TrivialError -> "'empty' parser from 'Alternative' is used"
    BiMapError biError -> prettyBiMapError biError
    KeyNotFound name -> "Key " <> joinKey name <> " is not found"
    TableNotFound name -> "Table [" <> joinKey name <> "] is not found"
    TypeMismatch name expected actual -> "Type for key " <> joinKey name <> " doesn't match."
        <> "\n  Expected: " <> expected
        <> "\n  Actual:   " <> Text.pack (showType actual)
    ParseError (ParseException msg) -> "Parse error during conversion from TOML to custom user type: \n  " <> msg
  where
    joinKey :: Key -> Text
    joinKey = Text.intercalate "." . map unPiece . toList . unKey

-- | File loading error data type.
data LoadTomlException = LoadTomlException !FilePath !Text

instance Show LoadTomlException where
    show (LoadTomlException filePath msg) = "Couldnt parse file " ++ filePath ++ ": " ++ show msg

instance Exception LoadTomlException
