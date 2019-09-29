{-# LANGUAGE DeriveAnyClass #-}

-- | Parser for text to TOML AST.

module Toml.Parser
       ( ParseException (..)
       , parse
       ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)

import Toml.Parser.Item (tomlP)
import Toml.Parser.Validate (validateItems)
import Toml.Type (TOML)

import qualified Data.Text as T
import qualified Toml.Parser.Core as P (errorBundlePretty, parse)


-- | Pretty parse exception for parsing toml.
newtype ParseException = ParseException
    { unParseException :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, NFData)

{- | Parses 'Text' as 'TOML' AST object. If you want to convert 'Text' to your
custom haskell data type, use 'Toml.Bi.Code.decode' or 'Toml.Bi.Code.decodeFile'
functions.
-}
parse :: Text -> Either ParseException TOML
parse t = case P.parse tomlP "" t of
    Left err    -> Left $ ParseException $ T.pack $ P.errorBundlePretty err
    Right items -> case validateItems items of
        Left err   -> Left $ ParseException $ T.pack $ show err
        Right toml -> Right toml
