module Benchmark.Htoml
       ( decodeHtoml
       , convertHtoml
       ) where

import Data.Aeson.Types (parseEither, parseJSON)
import Data.Text (Text)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types (Table)
import Benchmark.Type (HaskellType (..))

import qualified Text.Toml.Types as TTT

-- | Decode toml file to Haskell type.
decodeHtoml :: Text -> Either String HaskellType
decodeHtoml txt = case parseTomlDoc "" txt of
    Left _ -> error "Parsing failed"
    Right toml -> convertHtoml toml

-- | Convert from already parsed toml to Haskell type.
convertHtoml :: Table -> Either String HaskellType
convertHtoml = parseEither parseJSON . TTT.toJSON