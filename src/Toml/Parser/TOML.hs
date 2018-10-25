module Toml.Parser.TOML
       ( keyValP
       , tableHeaderP
       , tomlP
       ) where

import Control.Applicative (Alternative (many))

import Toml.Parser.Core (Parser, sc, text)
import Toml.Parser.Value (keyP, tableNameP, valueP)
import Toml.PrefixTree (Key (..), fromList)
import Toml.Type (AnyValue, TOML (..), typeCheck)

import qualified Data.HashMap.Lazy as HashMap


keyValP :: Parser (Key, AnyValue)
keyValP = do
    k    <- keyP
    _    <- text "="
    uval <- valueP
    case typeCheck uval of
        Left err -> fail $ show err
        Right v  -> pure (k, v)


tableHeaderP :: Parser (Key, TOML)
tableHeaderP = do
    k    <- tableNameP
    toml <- makeToml <$> many keyValP
    pure (k, toml)
  where
    makeToml :: [(Key, AnyValue)] -> TOML
    makeToml kv = TOML (HashMap.fromList kv) mempty


tomlP :: Parser TOML
tomlP = do
    sc
    kvs    <- many keyValP
    tables <- many tableHeaderP
    pure TOML { tomlPairs  = HashMap.fromList kvs
              , tomlTables = fromList tables
              }
