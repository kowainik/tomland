{- | Parser of TOML language. Implemented with the help of @megaparsec@ package. -}

module Toml.Parser
       ( ParseException (..)
       , parse
       , arrayP
       , boolP
       , doubleP
       , integerP
       , keyP
       , keyValP
       , textP
       , tableHeaderP
       , tomlP
       ) where

-- I hate default Prelude... Do I really need to import all this stuff manually?..
import Control.Applicative (Alternative (..))
import Control.Applicative.Combinators (between, manyTill, option, sepEndBy, skipMany)
import Control.Monad (void)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseErrorPretty', try)
import Text.Megaparsec.Char (alphaNumChar, anyChar, char, char', digitChar, oneOf, space1)

import Toml.PrefixTree (Key (..), Piece (..), fromList)
import Toml.Type (AnyValue, TOML (..), UValue (..), typeCheck)

import qualified Control.Applicative.Combinators.NonEmpty as NC
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega (parse)
import qualified Text.Megaparsec.Char.Lexer as L

----------------------------------------------------------------------------
-- Library for parsing (boilerplate copy-pasted from megaparsec tutorial)
----------------------------------------------------------------------------

type Parser = Parsec Void Text

-- space consumer
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "#"
    blockComment = empty

-- wrapper for consuming spaces after every lexeme (not before it!)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parser for "fixed" string
text :: Text -> Parser Text
text = L.symbol sc

text_ :: Text -> Parser ()
text_ = void . text

doubleP :: Parser Double
doubleP = L.signed sc $ lexeme L.float

----------------------------------------------------------------------------
-- TOML parser
----------------------------------------------------------------------------

-- Keys

bareKeyP :: Parser Text
bareKeyP = lexeme $ Text.pack <$> bareStrP
  where
    bareStrP :: Parser String
    bareStrP = some $ alphaNumChar <|> char '_' <|> char '-'

literalStringP :: Parser Text
literalStringP = lexeme $ Text.pack <$> (char '\'' *> anyChar `manyTill` char '\'')

-- TODO: this parser is incorrect, it doesn't recognize all strings
basicStringP :: Parser Text
basicStringP = lexeme $ Text.pack <$> (char '"' *> anyChar `manyTill` char '"')

textP :: Parser Text
textP = literalStringP <|> basicStringP

-- adds " or ' to both sides
quote :: Text -> Text -> Text
quote q t = q <> t <> q

keyComponentP :: Parser Piece
keyComponentP = Piece <$> (bareKeyP <|> (quote "\"" <$> basicStringP) <|> (quote "'" <$> literalStringP))

keyP :: Parser Key
keyP = Key <$> NC.sepBy1 keyComponentP (char '.')

tableNameP :: Parser Key
tableNameP = lexeme $ between (char '[') (char ']') keyP

-- Values

integerP :: Parser Integer
integerP = lexeme $ binary <|> octal <|> hexadecimal <|> decimal
  where
    decimal      = L.signed sc L.decimal
    binary       = try (char '0' >> char 'b') >> mkNum 2 <$> (some binDigitChar)
    octal        = try (char '0' >> char 'o') >> L.octal
    hexadecimal  = try (char '0' >> char 'x') >> L.hexadecimal
    binDigitChar = oneOf ['0', '1']
    mkNum b      = foldl' (step b) 0
    step b a c   = a * b + fromIntegral (digitToInt c)

floatP :: Parser Double
floatP = lexeme $ inf <|> nan <|> floatNum
  where
    inf :: Parser Double
    inf = try $ L.signed sc $ 1 / 0 <$ text "inf"
    nan :: Parser Double
    nan = try $ L.signed sc $ 0 / 0 <$ text "nan"
    floatNum :: Parser Double
    floatNum = fmap rd $ try (decimal <++> expo) <|> try (decimal <++> frac <++> option "" expo)
      where
        (<++>) a b = (++) <$> a <*> b
        (<:>)  a b = (:)  <$> a <*> b
        rd      = read :: String -> Double
        decimal = show <$> (L.signed sc L.decimal :: Parser Integer)
        frac    = char '.' <:> (some digitChar)
        expo    = char' 'e' <:> decimal

boolP :: Parser Bool
boolP = False <$ text "false"
    <|> True  <$ text "true"

-- dateTimeP :: Parser DateTime
-- dateTimeP = error "Not implemented!"

arrayP :: Parser [UValue]
arrayP = lexeme $ between (char '[' *> sc) (char ']') elements
  where
    elements :: Parser [UValue]
    elements = valueP `sepEndBy` spComma <* skipMany (text ",")

    spComma :: Parser ()
    spComma = char ',' *> sc

valueP :: Parser UValue
valueP = UBool    <$> boolP
     <|> UDouble  <$> try doubleP
     <|> UInteger <$> integerP
     <|> UText    <$> textP
--     <|> UDate   <$> dateTimeP
     <|> UArray   <$> arrayP

-- TOML

keyValP :: Parser (Key, AnyValue)
keyValP = do
    k <- keyP
    text_ "="
    uval <- valueP
    case typeCheck uval of
        Left err -> fail $ show err
        Right v  -> pure (k, v)

tableHeaderP :: Parser (Key, TOML)
tableHeaderP = do
    k <- tableNameP
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

---------------------------------------------------------------------------
-- Exposed API
----------------------------------------------------------------------------

-- | Pretty parse exception for parsing toml.
newtype ParseException = ParseException Text
    deriving (Show, Eq)

-- | Parses 'Text' as 'TOML' object.
parse :: Text -> Either ParseException TOML
parse t = case Mega.parse tomlP "" t of
    Left err   -> Left $ ParseException $ Text.pack $ parseErrorPretty' t err
    Right toml -> Right toml
