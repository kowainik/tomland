{- | Parser for 'TOML' data type: keys, tables, array of tables. Uses value
parsers from "Toml.Parser.Value".
-}

module Toml.Parser.TOML
       ( keyP
       , hasKeyP
       , getToml
       , tableArrayP
       , inlineTableP
       , tomlP
       ) where

import Control.Applicative (Alternative (..))
import Control.Monad.Combinators (between, eitherP, sepEndBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup ((<>))
import Data.Text (Text)

import Toml.Parser.Core (Parser, alphaNumChar, char, eof, lexeme, sc, text, try)
import Toml.Parser.String (basicStringP, literalStringP)
import Toml.Parser.Value (anyValueP)
import Toml.PrefixTree (Key (..), KeysDiff (..), Piece (..), fromList, keysDiff)
import Toml.Type (AnyValue, TOML (..))

import qualified Control.Applicative.Combinators.NonEmpty as NC
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text


-- | Parser for bare key piece, like @foo@.
bareKeyPieceP :: Parser Text
bareKeyPieceP = lexeme $ Text.pack <$> bareStrP
  where
    bareStrP :: Parser String
    bareStrP = some $ alphaNumChar <|> char '_' <|> char '-'

-- | Parser for 'Piece'.
keyComponentP :: Parser Piece
keyComponentP = Piece <$>
    (bareKeyPieceP <|> (quote "\"" <$> basicStringP) <|> (quote "'" <$> literalStringP))
  where
    -- adds " or ' to both sides
    quote :: Text -> Text -> Text
    quote q t = q <> t <> q

-- | Parser for 'Key': dot-separated list of 'Piece'.
keyP :: Parser Key
keyP = Key <$> keyComponentP `NC.sepBy1` char '.'

-- | Parser for table name: 'Key' inside @[]@.
tableNameP :: Parser Key
tableNameP = between (text "[") (text "]") keyP

-- | Parser for array of tables name: 'Key' inside @[[]]@.
tableArrayNameP :: Parser Key
tableArrayNameP = between (text "[[") (text "]]") keyP

-- | Parser for lines starting with 'key =', either values or inline tables.
hasKeyP :: Parser (Key, Either AnyValue TOML)
hasKeyP = (,) <$> keyP <* text "=" <*> eitherP anyValueP inlineTableP

-- | Parser for inline tables.
inlineTableP :: Parser TOML
inlineTableP = between
    (text "{") (text "}")
    (tomlFromInline <$> hasKeyP `sepEndBy` text ",")

-- | Parser for an array of tables under a certain key.
tableArrayP :: Key -> Parser (NonEmpty TOML)
tableArrayP key =
    getToml (Just key) `NC.sepBy1` sameKeyP key tableArrayNameP

-- | Parser for a '.toml' file
tomlP :: Parser TOML
tomlP = sc *> getToml Nothing <* eof

-- | Parser for a table under a certain key
getToml :: Maybe Key -> Parser TOML
getToml key = do
    (val, inline)  <- distributeEithers <$> many hasKeyP
    (table, array) <- distributeEithers <$> many (eitherPairP subTable subArray)
    pure TOML
        { tomlPairs       = HashMap.fromList val
        , tomlTables      = fromList $ inline ++ table
        , tomlTableArrays = HashMap.fromList array
        }
  where
    subTable :: Parser (Key, TOML)
    subTable = do
        (kDiff, k) <- try $ childKeyP key tableNameP
        t <- getToml (Just k)
        pure (kDiff, t)

    subArray :: Parser (Key, NonEmpty TOML)
    subArray = do
        (kDiff, k) <- try $ childKeyP key tableArrayNameP
        a <- tableArrayP k
        pure (kDiff, a)

-- | @childKeyP (Just key) p@ checks if the result of @p@ if a child key of
-- @key@ and returns the difference of the keys and the child key.
-- @childKeyP Nothing p@ is only called from @tomlP@ (no parent key).
childKeyP :: Maybe Key -> Parser Key -> Parser (Key, Key)
childKeyP Nothing parser = (\k -> (k, k)) <$> parser
childKeyP (Just key) parser = do
    k <- parser
    case keysDiff key k of
        FstIsPref d -> pure (d, k)
        _           -> fail $ show k ++ " is not a child key of " ++ show key

-- | @sameKeyP key p@ returns the result of @p@ if the key returned by @p@ is
-- the same as @key@, and fails otherwise.
sameKeyP :: Key -> Parser Key -> Parser Key
sameKeyP key parser = try $ do
    k <- parser
    case keysDiff key k of
        Equal -> pure k
        _     -> fail $ show k ++ " is not the same as " ++ show key

-- Helper functions

-- | Helper function to create a 'TOML' from a list of key/values or inline tables.
tomlFromInline :: [(Key, Either AnyValue TOML)] -> TOML
tomlFromInline kvs =
    let (lefts, rights) = distributeEithers kvs
    in TOML (HashMap.fromList lefts) (fromList rights) mempty

-- | Helper function to seperate the 'Either'.
distributeEithers :: [(c, Either a b)] -> ([(c, a)], [(c, b)])
distributeEithers = foldr distribute ([], [])
  where
    distribute :: (c, Either a b) -> ([(c, a)], [(c, b)]) -> ([(c, a)], [(c, b)])
    distribute (k, Left a) (ls, rs)  = ((k, a) : ls, rs)
    distribute (k, Right b) (ls, rs) = (ls, (k, b) : rs)

-- | Helper function to make an 'Either' parser.
eitherPairP :: Alternative m => m (c, a) -> m (c, b) -> m (c, Either a b)
eitherPairP a b = (fmap Left <$> a) <|> (fmap Right <$> b)
