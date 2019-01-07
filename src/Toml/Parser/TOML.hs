module Toml.Parser.TOML
       ( hasKeyP
       , tableP
       , tableArrayP
       , inlineTableP
       , tomlP
       ) where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Monad.Combinators (between, eitherP, optional, sepEndBy)

import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Toml.Parser.Core (Parser, sc, text, try)
import Toml.Parser.Value (anyValueP, keyP, tableArrayNameP, tableNameP)
import Toml.PrefixTree (Key (..), KeysDiff (..), fromList, keysDiff)
import Toml.Type (AnyValue, TOML (..))

import qualified Data.HashMap.Lazy as HashMap

-- Parsers

-- | Parser for lines starting with 'key =', either values or inline tables
hasKeyP :: Parser (Key, Either AnyValue TOML)
hasKeyP = (,) <$> keyP <* text "=" <*> eitherP anyValueP inlineTableP

-- | Parser for inline tables
inlineTableP :: Parser TOML
inlineTableP = between (text "{") (text "}")
                       (makeToml <$> hasKeyP `sepEndBy` text ",")

-- | Parser for a table
tableP :: Parser (Key, TOML)
tableP = do
  key <- tableNameP
  toml <- subTableContent key
  return (key, toml)

-- | Parser for an array of tables
tableArrayP :: Parser (Key, NonEmpty TOML)
tableArrayP = do
  key <- tableArrayNameP
  localToml <- subTableContent key
  more <- optional $ sameKeyP key tableArrayP
  case more of
    Nothing         -> return (key, localToml :| [])
    Just (_, tomls) -> return (key, localToml <| tomls)

-- | Parser for a '.toml' file
tomlP :: Parser TOML
tomlP = do
  sc
  (val, inline) <- distributeEithers <$> many hasKeyP
  (table, array) <- fmap distributeEithers $ many $
                    eitherPairP (try tableP) tableArrayP
  return TOML { tomlPairs  = HashMap.fromList val
              , tomlTables = fromList $ inline ++ table
              , tomlTableArrays = HashMap.fromList array
              }

-- | Parser for full 'TOML' under a certain key
subTableContent :: Key -> Parser TOML
subTableContent key = do
  (val, inline) <- distributeEithers <$> many hasKeyP
  (table, array) <- fmap distributeEithers $ many $ childKeyP key $
                    eitherPairP (try tableP) tableArrayP
  return TOML { tomlPairs  = HashMap.fromList val
              , tomlTables = fromList $ inline ++ table
              , tomlTableArrays = HashMap.fromList array
              }

-- | 'childKeyP key p' returns the result of 'p' if the key returned by 'p' is
-- a child key of the 'key', and fails otherwise.
childKeyP :: Key -> Parser (Key, a) -> Parser (Key, a)
childKeyP key p = try $ do
  (k, x) <- p
  case keysDiff key k of
    FstIsPref k' -> return (k', x)
    _            -> fail $ show k ++ " is not a child key of " ++ show key

-- | 'sameKeyP key p' returns the result of 'p' if the key returned by 'p' is
-- the same as 'key', and fails otherwise.
sameKeyP :: Key -> Parser (Key, a) -> Parser (Key, a)
sameKeyP key parser = try $ do
  (k, x) <- parser
  case keysDiff key k of
    Equal -> return (k, x)
    _     -> fail $ show k ++ " is not the same as " ++ show key

-- Helper functions

-- | Helper function to create a 'TOML' from a list of key/values or inline tables
makeToml :: [(Key, Either AnyValue TOML)] -> TOML
makeToml kvs =
  let (lefts, rights) = distributeEithers kvs
  in TOML (HashMap.fromList lefts) (fromList rights) mempty

-- | Helper function to seperate the 'Either'
distributeEithers :: [(c, Either a b)] -> ([(c, a)], [(c, b)])
distributeEithers = foldr distribute ([], [])
  where
    distribute :: (c, Either a b) -> ([(c, a)], [(c, b)]) -> ([(c, a)], [(c, b)])
    distribute (k, Left a) (ls, rs)  = ((k, a) : ls, rs)
    distribute (k, Right b) (ls, rs) = (ls, (k, b) : rs)

-- | Helper function to make an 'Either' parser
eitherPairP :: Alternative m => m (c, a) -> m (c, b) -> m (c, Either a b)
eitherPairP a b = (fmap Left <$> a) <|> (fmap Right <$> b)
