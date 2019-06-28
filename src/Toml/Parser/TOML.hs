{- | Parser for 'TOML' data type: keys, tables, array of tables. Uses value
parsers from "Toml.Parser.Value".
-}

module Toml.Parser.TOML
       ( keyP
       , tomlP
       ) where

import Control.Applicative (Alternative (..))
import Control.Monad.Combinators (between, sepEndBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup ((<>))
import Data.Text (Text)

import Toml.Parser.Core (Parser, alphaNumChar, char, eof, lexeme, sc, text, try)
import Toml.Parser.String (basicStringP, literalStringP)
import Toml.Parser.Value (anyValueP)
import Toml.PrefixTree (Key (..), KeysDiff (..), Piece (..), keysDiff, single)
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

-- Helper functions for building TOML
tomlKV :: Key -> AnyValue -> TOML
tomlKV k v = mempty { tomlPairs = HashMap.singleton k v }

tomlT :: Key -> TOML -> TOML
tomlT k t = mempty { tomlTables = single k t }

tomlA :: Key -> NonEmpty TOML -> TOML
tomlA k a = mempty { tomlTableArrays = HashMap.singleton k a }

-- | Parser for lines starting with 'key =', either values, inline tables or
-- inline arrays of tables.
hasKeyP :: Maybe Key -> Parser TOML
hasKeyP key = do
    k <- keyP <* text "="
    table k <|> try (tableArray k) <|> (tomlKV k <$> anyValueP)
  where
    table :: Key -> Parser TOML
    table k = do
        (kDiff, _) <- childKeyP key (pure k)
        tomlT kDiff <$> inlineTableP

    tableArray :: Key -> Parser TOML
    tableArray k = do
        (kDiff, _) <- childKeyP key (pure k)
        tomlA kDiff <$> inlineTableArrayP

-- | Parser for inline tables.
inlineTableP :: Parser TOML
inlineTableP = between (text "{") (text "}") $
    mconcat <$>
    (tomlKV <$> (keyP <* text "=") <*> anyValueP ) `sepEndBy` text ","

-- | Parser for inline arrays of tables.
inlineTableArrayP :: Parser (NonEmpty TOML)
inlineTableArrayP = between (text "[") (text "]") $
    inlineTableP `NC.sepEndBy1` text ","

-- | Parser for an array of tables under a certain key.
tableArrayP :: Key -> Parser (NonEmpty TOML)
tableArrayP key =
    localTomlP (Just key) `NC.sepBy1` sameKeyP key tableArrayNameP

-- | Parser for a '.toml' file
tomlP :: Parser TOML
tomlP = sc *> localTomlP Nothing <* eof

-- | Parser for a toml under a certain key
localTomlP :: Maybe Key -> Parser TOML
localTomlP key = mconcat <$> many (subArray <|> subTable <|> hasKeyP key)
  where
    subTable :: Parser TOML
    subTable = do
        (kDiff, k) <- try $ childKeyP key tableNameP
        tomlT kDiff <$> localTomlP (Just k)

    subArray :: Parser TOML
    subArray = do
        (kDiff, k) <- try $ childKeyP key tableArrayNameP
        tomlA kDiff <$> tableArrayP k

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
