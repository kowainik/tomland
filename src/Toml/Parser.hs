{- | Parser of TOML language. Implemented with the help of @megaparsec@ package. -}

module Toml.Parser
       ( ParseException (..)
       , parse
       ) where

-- I hate default Prelude... Do I really need to import all this stuff manually?..
import Control.Applicative (Alternative (..), liftA2)
import Control.Applicative.Combinators (between, manyTill, sepBy)
import Control.Monad (void)
import Data.Function (on)
import Data.HashMap.Lazy (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ord (comparing)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseErrorPretty', try)
import Text.Megaparsec.Char (alphaNumChar, anyChar, char, space, space1)

import Toml.Type (AnyValue, Key (..), TOML (..), UValue (..), typeCheck)

import qualified Control.Applicative.Combinators.NonEmpty as NC
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List (isPrefixOf, sortBy)
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

integer :: Parser Integer
integer = L.signed sc (lexeme L.decimal)

float :: Parser Double
float = lexeme L.float

----------------------------------------------------------------------------
-- TOML parser
----------------------------------------------------------------------------

-- Keys

bareKeyP :: Parser Text
bareKeyP = lexeme $ Text.pack <$> bareStrP
  where
    bareStrP :: Parser String
    bareStrP = some $ alphaNumChar <|> char '_' <|> char '-'

stringP :: Parser Text
stringP = lexeme $ Text.pack <$> (char '"' *> anyChar `manyTill` char '"')

-- adds " to both sides
quote :: Text -> Text
quote t = "\"" <> t <> "\""

keyComponentP :: Parser Text
keyComponentP = bareKeyP <|> (quote <$> stringP)

keyP :: Parser Key
keyP = Key <$> NC.sepBy1 keyComponentP (char '.')

tableNameP :: Parser Key
tableNameP = lexeme $ between (char '[') (char ']') keyP

-- Values

boolP :: Parser Bool
boolP = False <$ text "false"
    <|> True  <$ text "true"

-- dateTimeP :: Parser DateTime
-- dateTimeP = error "Not implemented!"

arrayP :: Parser [UValue]
arrayP = lexeme $ between (char '[') (char ']') (valueP `sepBy` spComma)
  where
    spComma :: Parser ()
    spComma = char ',' *> space

valueP :: Parser UValue
valueP = UBool   <$> boolP
     <|> UInt    <$> try integer
     <|> UFloat  <$> float
     <|> UString <$> stringP
--     <|> UDate   <$> dateTimeP
     <|> UArray  <$> arrayP

-- TOML

keyValP :: Parser (Key, AnyValue)
keyValP = do
    k <- keyP
    text_ "="
    uval <- valueP
    case typeCheck uval of
        Nothing -> fail "Can't type check value!"
        Just v  -> pure (k, v)

data TableHeader = TableHeader
    { thName :: Key
    , thKeys :: [(Key, AnyValue)]
    }

tableHeaderP :: Parser TableHeader
tableHeaderP = liftA2 TableHeader tableNameP (many keyValP)

tomlP :: Parser TOML
tomlP = do
    kvs    <- many keyValP
    tables <- many tableHeaderP
    pure TOML { tomlPairs  = HashMap.fromList kvs
              , tomlTables = merge $ List.sortBy (comparing thName) tables
              }

----------------------------------------------------------------------------
-- Very ugly hack just to merge different tables grouping by biggest common prefix
----------------------------------------------------------------------------

-- assumes that passed list is sorted; fortunately, we need to sort this list only once
merge :: [TableHeader] -> HashMap Key TOML
merge = HashMap.fromList . map createTable . groupByPrefix
  where
    createTable :: NonEmpty TableHeader -> (Key, TOML)
    createTable (TableHeader{..} :| headers) =
      ( thName
      , TOML { tomlPairs  = HashMap.fromList thKeys
             , tomlTables = merge headers
             }
      )

{- Groups by prefix. So the following list:

["a", "a.x", "a.y", "b.1", "b.2", "c"]

will be converted in the following list of non empty lists:

[ "a" :| ["a.x", "a.y"], "b.1" :| [], "b.2" :| [], "c" :| [] ]

-}
groupByPrefix :: [TableHeader] -> [NonEmpty TableHeader]
groupByPrefix []     = []
groupByPrefix (x:xs) = let (y, ys) = span (isThPref x) xs in (x :| y) : groupByPrefix ys
  where
    isThPref :: TableHeader -> TableHeader -> Bool
    isThPref = isPrefixOf `on` unKey . thName

isPrefixOf :: Eq a => NonEmpty a -> NonEmpty a -> Bool
(x :| xs) `isPrefixOf` (y :| ys) = x == y && List.isPrefixOf xs ys

---------------------------------------------------------------------------
-- Exposed API
----------------------------------------------------------------------------

-- | Pretty parse exception for parsing toml.
data ParseException = ParseException Text
    deriving (Show)

-- | Parses 'Text' as 'TOML' object.
parse :: Text -> Either ParseException TOML
parse t = case Mega.parse tomlP "" t of
    Left err   -> Left $ ParseException $ Text.pack $ parseErrorPretty' t err
    Right toml -> Right toml
