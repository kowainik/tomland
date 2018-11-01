module Benchmark.Htoml
       ( decode
       , convert
       ) where

import Data.Aeson.Types (parseEither, parseJSON)
import Data.Text (Text)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types (Table, Node)
import Benchmark.Type (HaskellType (..))
import Control.DeepSeq (NFData, rnf, rwhnf)
import Text.Parsec.Error (ParseError)

import qualified Text.Toml.Types as TTT

-- | Decode toml file to Haskell type.
decode :: Text -> Either String HaskellType
decode txt = case parseTomlDoc "" txt of
    Left _ -> error "Parsing failed"
    Right toml -> convert toml

-- | Convert from already parsed toml to Haskell type.
convert :: Table -> Either String HaskellType
convert = parseEither parseJSON . TTT.toJSON

instance NFData Node where
    rnf = rwhnf

instance NFData ParseError where
    rnf = rwhnf