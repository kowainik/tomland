module Toml.Parser.TOML
       ( hasKeyP
       , tableHeaderP
       , inlineTableP
       , tomlP
       ) where

import Control.Applicative (Alternative (many))
import Control.Applicative.Combinators (sepEndBy, between, eitherP)

import Toml.Parser.Core (Parser, sc, text)
import Toml.Parser.Value (keyP, tableNameP, anyValueP)
import Toml.PrefixTree (Key (..), fromList)
import Toml.Type (AnyValue, TOML (..))

import qualified Data.HashMap.Lazy as HashMap

hasKeyP :: Parser (Key, Either AnyValue TOML)
hasKeyP = (,) <$> keyP <* text "=" <*> eitherP anyValueP inlineTableP

inlineTableP :: Parser TOML
inlineTableP = between (text "{") (text "}")
                       (makeToml <$> hasKeyP `sepEndBy` text ",")

tableHeaderP :: Parser (Key, TOML)
tableHeaderP = (,) <$> tableNameP <*> (makeToml <$> many hasKeyP)

distributeEithers :: [(c, Either a b)] -> ([(c, a)], [(c, b)])
distributeEithers = foldr distribute ([], [])
  where
    distribute :: (c, Either a b) -> ([(c, a)], [(c, b)]) -> ([(c, a)], [(c, b)])
    distribute (k, Left a) (ls, rs) = ((k, a) : ls, rs)
    distribute (k, Right b) (ls, rs) = (ls, (k, b) : rs)

makeToml :: [(Key, Either AnyValue TOML)] -> TOML
makeToml kvs = TOML (HashMap.fromList lefts) (fromList rights)
  where
    (lefts, rights) = distributeEithers kvs

tomlP :: Parser TOML
tomlP = do
    sc
    (val, inline) <- distributeEithers <$> many hasKeyP
    tables <- many tableHeaderP
    return TOML { tomlPairs  = HashMap.fromList val
                , tomlTables = fromList $ tables ++ inline
                }
