{- | Parser for 'TOML' data type: keys, tables, array of tables. Uses value
parsers from "Toml.Parser.Value".
-}

module Toml.Parser.TOML
       ( keyP
       , hasKeyP
       , tableP
       , tableArrayP
       , tomlP
       ) where

import Control.Applicative (Alternative (..))
import Control.Monad.Combinators (between, optional, sepEndBy, sepEndBy1)
import Data.Semigroup ((<>))
import Data.Text (Text)

import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Toml.Parser.Core (Parser, alphaNumChar, char, eof, lexeme, sc, text, try)
import Toml.Parser.String (basicStringP, literalStringP)
import Toml.Parser.Value (anyValueP)
import Toml.PrefixTree (Key (..), KeysDiff (..), Piece (..), keysDiff, single)
import Toml.Type (TOML (..))

import qualified Control.Applicative.Combinators.NonEmpty as NC
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List.NonEmpty as NE
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
keyP = Key <$> NC.sepBy1 keyComponentP (char '.')

-- | Parser for table name: 'Key' inside @[]@.
tableNameP :: Parser Key
tableNameP = between (text "[") (text "]") keyP

-- | Parser for array of tables name: 'Key' inside @[[]]@.
tableArrayNameP :: Parser Key
tableArrayNameP = between (text "[[") (text "]]") keyP

-- | Parser for lines starting with 'key =', either values or inline tables.
hasKeyP :: Parser TOML
hasKeyP = keyP <* text "="
    >>= \k -> inlineTableP k <|> try (inlineArrayP k) <|> keyValueP k

-- | Parser for key/value pairs.
keyValueP :: Key -> Parser TOML
keyValueP k = anyValueP
    >>= \v -> pure $ TOML (HashMap.singleton k v) mempty mempty

-- | Parser for inline tables.
inlineTableP :: Key -> Parser TOML
inlineTableP k = between
    (text "{") (text "}")
    (mconcat <$> hasKeyP `sepEndBy` text ",")
    >>= \v -> pure $ TOML mempty (single k v) mempty

-- | Parser for inline arrays of tables.
inlineArrayP :: Key -> Parser TOML
inlineArrayP k = between
    (text "[") (text "]")
    (NE.fromList <$> inlineTableP k `sepEndBy1` text ",")
    >>= \v -> pure $ TOML mempty mempty (HashMap.singleton k v)

-- | Parser for a table.
tableP :: Parser (Key, TOML)
tableP = do
    key  <- tableNameP
    toml <- subTableContent key
    pure (key, TOML mempty (single key toml) mempty)

-- | Parser for an array of tables.
tableArrayP :: Parser (Key, TOML)
tableArrayP = do
    key       <- tableArrayNameP
    localToml <- subTableContent key
    more      <- optional $ sameKeyP key tableArrayP
    case more of
        Nothing         -> pure (key, TOML mempty mempty (HashMap.singleton key (localToml :| [])))
        Just (_, tomls) -> pure (key, TOML mempty mempty (HashMap.singleton key (localToml <| (tomlTableArrays tomls HashMap.! key))))

-- | Parser for a '.toml' file
tomlP :: Parser TOML
tomlP = do
    sc
    hasKey <- many hasKeyP
    noKey  <- many $ try tableP <|> tableArrayP
    eof
    pure $ mconcat $ hasKey <> map snd noKey

-- | Parser for full 'TOML' under a certain key
subTableContent :: Key -> Parser TOML
subTableContent key = do
    hasKey <- many hasKeyP
    noKey  <- many $ childKeyP key $ try tableP <|> tableArrayP
    pure $ mconcat $ hasKey <> map snd noKey

-- | @childKeyP key p@ pures the result of @p@ if the key pureed by @p@ is
-- a child key of the @key@, and fails otherwise.
childKeyP :: Key -> Parser (Key, a) -> Parser (Key, a)
childKeyP key p = try $ do
    (k, x) <- p
    case keysDiff key k of
        FstIsPref k' -> pure (k', x)
        _            -> fail $ show k ++ " is not a child key of " ++ show key

-- | @sameKeyP key p@ pures the result of @p@ if the key pureed by @p@ is
-- the same as @key@, and fails otherwise.
sameKeyP :: Key -> Parser (Key, a) -> Parser (Key, a)
sameKeyP key parser = try $ do
    (k, x) <- parser
    case keysDiff key k of
        Equal -> pure (k, x)
        _     -> fail $ show k ++ " is not the same as " ++ show key
