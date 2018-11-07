{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE PackageImports #-}

module Benchmark.HtomlMegaparsec
       ( convert
       , decode
       , parse
       ) where

import Data.Aeson.Types (ToJSON, parseEither, parseJSON, toJSON)
import Data.Text (Text)
import "htoml-megaparsec" Text.Toml (Node (..), Table, TomlError, parseTomlDoc)

import Benchmark.Type (HaskellType)


-- | Wrapper on htoml-megaparsec's parseTomlDoc
parse :: FilePath -> Text -> Either TomlError Table
parse _ = parseTomlDoc ""

-- | Convert from already parsed toml to Haskell type.
convert :: Table -> Either String HaskellType
convert = parseEither parseJSON . toJSON

-- | Decode toml file to Haskell type.
decode :: Text -> Either String HaskellType
decode txt = case parseTomlDoc "log" txt of
    Left _     -> error "Parsing failed"
    Right toml -> convert toml

-- | 'ToJSON' instances for the 'Node' type that produce Aeson (JSON)
-- in line with the TOML specification.
instance ToJSON Node where
    toJSON (VTable v)    = toJSON v
    toJSON (VTArray v)   = toJSON v
    toJSON (VString v)   = toJSON v
    toJSON (VInteger v)  = toJSON v
    toJSON (VFloat v)    = toJSON v
    toJSON (VBoolean v)  = toJSON v
    toJSON (VDatetime v) = toJSON v
    toJSON (VArray v)    = toJSON v
