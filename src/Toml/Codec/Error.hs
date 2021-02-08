{-# LANGUAGE DeriveAnyClass #-}

{- |
Module                  : Toml.Codec.Error
Copyright               : (c) 2018-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Core error types, including 'TomlDecodeError' and 'LoadTomlException'.

@since 1.3.0.0
-}

module Toml.Codec.Error
    ( TomlDecodeError (..)
    , prettyTomlDecodeErrors
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

import qualified Data.Text as Text


{- | Type of exception for converting from TOML to user custom data type.

@since 1.3.0.0
-}
data TomlDecodeError
    = BiMapError !Key !TomlBiMapError
    | KeyNotFound !Key  -- ^ No such key
    | TableNotFound !Key  -- ^ No such table
    | TableArrayNotFound !Key
      {- ^ No such table array

      @since 1.3.0.0
      -}
    | ParseError !TomlParseError  -- ^ Exception during parsing
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

{- | Converts 'TomlDecodeError's into pretty human-readable text.

@since 1.3.0.0
-}
prettyTomlDecodeErrors :: [TomlDecodeError] -> Text
prettyTomlDecodeErrors errs = Text.unlines $
    ("tomland errors number: " <> Text.pack (show $ length errs))
    : map prettyTomlDecodeError errs

{- | Converts 'TomlDecodeError' into pretty human-readable text.

@since 1.3.0.0
-}
prettyTomlDecodeError :: TomlDecodeError -> Text
prettyTomlDecodeError de = "tomland decode error:  " <> case de of
    BiMapError name biError -> "BiMap error in key '" <> prettyKey name <> "' : "
        <> prettyBiMapError biError
    KeyNotFound name -> "Key " <> prettyKey name <> " is not found"
    TableNotFound name -> "Table [" <> prettyKey name <> "] is not found"
    TableArrayNotFound name -> "Table array [[" <> prettyKey name <> "]] is not found"
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
