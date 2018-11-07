{-# LANGUAGE DeriveAnyClass #-}

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

-- | Parses 'Text' as 'TOML' object.
parse :: Text -> Either ParseException TOML
parse t = case P.parse tomlP "" t of
    Left err   -> Left $ ParseException $ pack $ P.errorBundlePretty err
    Right toml -> Right toml
