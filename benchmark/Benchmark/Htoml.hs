{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Benchmark.Htoml
       ( decode
       , parse
       , convert
       ) where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Parsec.Error (ParseError)

import "htoml" Text.Toml (parseTomlDoc)
import "htoml" Text.Toml.Types (Node (..), Table, toJSON)

import Benchmark.Type (HaskellType)


-- | Decode toml file to Haskell type.
decode :: Text -> Either String HaskellType
decode txt = case parseTomlDoc "" txt of
    Left err   -> error $ "'htoml' parsing failed: " <> show err
    Right toml -> convert toml

-- | Wrapper on htoml's 'parseTomlDoc'
parse :: Text -> Either ParseError Table
parse = parseTomlDoc ""

-- | Convert from already parsed toml to Haskell type.
convert :: Table -> Either String HaskellType
convert = parseEither parseJSON . toJSON

deriving instance NFData Node
deriving instance Generic Node

instance NFData ParseError where
    rnf = rwhnf
