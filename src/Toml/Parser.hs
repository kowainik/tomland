module Toml.Parser
       ( parse
       ) where

import Data.Text (Text)

import Toml.Type (TOML)

-- | Dummy parse exception for now.
data ParseException = ParseException

parse :: Text -> Either ParseException TOML
parse = error "Not implemented"
