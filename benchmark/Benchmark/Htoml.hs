{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Benchmark.Htoml
       ( decode
       , convert
       , parse
       ) where

import Benchmark.Type (HaskellType)
import Control.DeepSeq (NFData, rnf, rwhnf)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Parsec.Error (ParseError)

import "htoml" Text.Toml (parseTomlDoc)
import "htoml" Text.Toml.Types (Node (..), Table, toJSON)


-- | Decode toml file to Haskell type.
decode :: Text -> Either String HaskellType
decode txt = case parseTomlDoc "" txt of
    Left _     -> error "Parsing failed"
    Right toml -> convert toml

-- | Convert from already parsed toml to Haskell type.
convert :: Table -> Either String HaskellType
convert = parseEither parseJSON . toJSON

-- | Wrapper on htoml's parseTomlDoc
parse :: Text -> Either ParseError Table
parse = parseTomlDoc ""

deriving instance NFData Node
deriving instance Generic Node

instance NFData ParseError where
    rnf = rwhnf
