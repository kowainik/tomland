{-# LANGUAGE DeriveAnyClass #-}

-- | Parser for text to TOML AST.

module Toml.Parser
       ( ParseException (..)
       , parse
       ) where

import Control.DeepSeq (NFData)
import Data.Text (Text, pack)
import GHC.Generics (Generic)

import Toml.Parser.TOML (tomlP)
import Toml.Type (TOML)

import qualified Toml.Parser.Core as P (errorBundlePretty, parse)


-- | Pretty parse exception for parsing toml.
newtype ParseException = ParseException Text
    deriving (Show, Eq, Generic, NFData)

{- | Parses 'Text' as 'TOML' AST object. If you want to convert 'Text' to your
custom haskell data type, use 'Toml.Bi.Code.decode' or 'Toml.Bi.Code.decodeFile'
functions.
-}
parse :: Text -> Either ParseException TOML
parse t = case P.parse tomlP "" t of
    Left err   -> Left $ ParseException $ pack $ P.errorBundlePretty err
    Right toml -> Right toml
