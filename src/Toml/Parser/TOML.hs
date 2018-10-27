module Toml.Parser.TOML
       ( keyValP
       , tableHeaderP
       , inlineTableP
       , tomlP
       ) where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Applicative.Combinators (sepEndBy, between)
import Data.Either (lefts, rights)

import Toml.Parser.Core (Parser, sc, text, char, try)
import Toml.Parser.Value (keyP, tableNameP, anyValueP)
import Toml.PrefixTree (Key (..), fromList)
import Toml.Type (AnyValue, TOML (..))

import qualified Data.HashMap.Lazy as HashMap

hasKeyP :: Parser (Either (Key, AnyValue) (Key, TOML))
hasKeyP = try (Left <$> keyValP) <|> (Right <$> inlineTableP)

keyValP :: Parser (Key, AnyValue)
keyValP = (,) <$> keyP <* text "=" <*> anyValueP

inlineTableP :: Parser (Key, TOML)
inlineTableP =
  (,) <$> keyP <* text "="
      <*> between (chSc '{') (chSc '}')
                  (makeToml <$> hasKeyP `sepEndBy` chSc ',')
  where
    chSc :: Char -> Parser ()
    chSc c = char c *> sc

tableHeaderP :: Parser (Key, TOML)
tableHeaderP = (,) <$> tableNameP <*> (makeToml <$> many hasKeyP)

makeToml :: [Either (Key, AnyValue) (Key, TOML)] -> TOML
makeToml kvs = TOML (HashMap.fromList $ lefts kvs) (fromList $ rights kvs)

tomlP :: Parser TOML
tomlP = do
    sc
    kvs    <- many hasKeyP
    tables <- many tableHeaderP
    return TOML { tomlPairs  = HashMap.fromList $ lefts kvs
                , tomlTables = fromList $ tables ++ rights kvs
                }
