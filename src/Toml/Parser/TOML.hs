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
import Toml.Type (TOML (..))

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

-- Helper for building TOML
toml :: TOML
toml = mempty

-- | Parser for lines starting with 'key =', either values, inline tables or
-- inline arrays of tables.
hasKeyP :: Maybe Key -> Parser TOML
hasKeyP key = do
    k <- keyP <* text "="
    try (tableArray key k) <|> try (table key k) <|> keyValue k
  where
    keyValue :: Key -> Parser TOML
    keyValue k = do
      v <- anyValueP
      pure $ toml { tomlPairs = HashMap.singleton k v}

    table :: Maybe Key -> Key -> Parser TOML
    table Nothing k = do
      t <- inlineTableP
      pure $ toml {tomlTables = single k t}
    table (Just parentKey) childKey = do
        case keysDiff parentKey childKey of
            FstIsPref d -> do
                t <- inlineTableP
                pure $ toml {tomlTables = single d t}
            _           ->
                fail $ show childKey ++ " is not a child key of " ++ show parentKey

    tableArray :: Maybe Key -> Key -> Parser TOML
    tableArray Nothing k = do
      a <- inlineTableArrayP
      pure $ toml {tomlTableArrays = HashMap.singleton k a}
    tableArray (Just parentKey) childKey =
        case keysDiff parentKey childKey of
            FstIsPref d -> do
                a <- inlineTableArrayP
                pure $ toml {tomlTableArrays = HashMap.singleton d a}
            _           ->
                fail $ show childKey ++ " is not a child key of " ++ show parentKey

-- | Parser for inline tables.
inlineTableP :: Parser TOML
inlineTableP = between (text "{") (text "}") $ do
  kv <- ((,) <$> (keyP <* text "=") <*> anyValueP) `sepEndBy` text ","
  pure $ toml { tomlPairs = HashMap.fromList kv}

-- | Parser for inline arrays of tables.
inlineTableArrayP :: Parser (NonEmpty TOML)
inlineTableArrayP = between
    (text "[") (text "]")
    (inlineTableP `NC.sepEndBy1` text ",")

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
      t <- localTomlP (Just k)
      pure $ toml {tomlTables = single kDiff t}

    subArray :: Parser TOML
    subArray = do
      (kDiff, k) <- try $ childKeyP key tableArrayNameP
      a <- tableArrayP k
      pure $ toml {tomlTableArrays = HashMap.singleton kDiff a}

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
