{- |
Module                  : Toml.Parser
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Parser for text to TOML AST.

@since 0.0.0
-}

module Toml.Parser
       ( TomlParseError (..)
       , parse
       , parseKey
       ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)

import Toml.Parser.Item (tomlP)
import Toml.Parser.Key (keyP)
import Toml.Parser.Validate (validateItems)
import Toml.Type.Key (Key)
import Toml.Type.TOML (TOML)

import qualified Data.Text as T
import qualified Toml.Parser.Core as P (errorBundlePretty, parse)


{- | Pretty parse exception for parsing toml.

@since 1.3.0.0
-}
newtype TomlParseError = TomlParseError
    { unTomlParseError :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, NFData)

{- | Parses 'Data.Text.Text' as 'TOML' AST object. If you want to
convert 'Data.Text.Text' to your custom haskell data type, use
'Toml.Codec.Code.decode' or 'Toml.Codec.Code.decodeFile' functions.

@since 0.0.0
-}
parse :: Text -> Either TomlParseError TOML
parse t = case P.parse tomlP "" t of
    Left err    -> Left $ TomlParseError $ T.pack $ P.errorBundlePretty err
    Right items -> case validateItems items of
        Left err   -> Left $ TomlParseError $ T.pack $ show err
        Right toml -> Right toml

{- | Parse TOML 'Key' from 'Text'.

@since 1.3.0.0
-}
parseKey :: Text -> Either TomlParseError Key
parseKey t = case P.parse keyP "" t of
    Left err  -> Left $ TomlParseError $ T.pack $ P.errorBundlePretty err
    Right key -> Right key
