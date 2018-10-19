{- | Parser of TOML language. Implemented with the help of @megaparsec@ package. -}

module Toml.Parser
       ( ParseException (..)
       , parse
       , arrayP
       , boolP
       , dateTimeP
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
import Control.Applicative.Combinators (between, count, manyTill, optional, sepEndBy, skipMany)
import Control.Monad (void)
import Data.Char (chr, isControl)
import Data.Either (fromRight)
import Data.Fixed (Pico)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Time (LocalTime (..), ZonedTime (..), fromGregorianValid, makeTimeOfDayValid,
                  minutesToTimeZone)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, errorBundlePretty, match, satisfy, try, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, eol, hexDigitChar, space, space1,
                             string, tab)

import Toml.PrefixTree (Key (..), Piece (..), fromList)
import Toml.Type (AnyValue, DateTime (..), TOML (..), UValue (..), typeCheck)

import qualified Control.Applicative.Combinators.NonEmpty as NC
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Read as TR
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

----------------------------------------------------------------------------
-- TOML parser
----------------------------------------------------------------------------

-- Strings

textP :: Parser Text
textP = multilineBasicStringP <|> multilineLiteralStringP <|> literalStringP <|> basicStringP <?> "text"

nonControlCharP :: Parser Text
nonControlCharP = Text.singleton <$> satisfy (not . isControl)

escapeSequenceP :: Parser Text
escapeSequenceP = char '\\' >> anySingle >>= \case
                    'b'  -> pure "\b"
                    't'  -> pure "\t"
                    'n'  -> pure "\n"
                    'f'  -> pure "\f"
                    'r'  -> pure "\r"
                    '"'  -> pure "\""
                    '\\' -> pure "\\"
                    'u'  -> hexUnicodeP 4
                    'U'  -> hexUnicodeP 8
                    c    -> fail $ "Invalid escape sequence: " <> "\\" <> [c]
  where
    hexUnicodeP :: Int -> Parser Text
    hexUnicodeP n = count n hexDigitChar
                    >>= \x -> case toUnicode $ hexToInt x of
                          Just c  -> pure (Text.singleton c)
                          Nothing -> fail $ "Invalid unicode character: " <> "\\" <> (if n == 4 then "u" else "U") <> x
      where
        hexToInt :: String -> Int
        hexToInt xs = read $ "0x" ++ xs

        toUnicode :: Int -> Maybe Char
        toUnicode x
          -- Ranges from "The Unicode Standard".
          -- See definition D76 in Section 3.9, Unicode Encoding Forms.
          | x >= 0      && x <= 0xD7FF   = Just (chr x)
          | x >= 0xE000 && x <= 0x10FFFF = Just (chr x)
          | otherwise                    = Nothing

basicStringP :: Parser Text
basicStringP = lexeme $ mconcat <$> (char '"' *> (escapeSequenceP <|> nonControlCharP) `manyTill` char '"')

literalStringP :: Parser Text
literalStringP = lexeme $ Text.pack <$> (char '\'' *> nonEolCharP `manyTill` char '\'')
  where
    nonEolCharP :: Parser Char
    nonEolCharP = satisfy (\c -> c /= '\n' && c /= '\r')

multilineP :: Parser Text -> Parser Text -> Parser Text
multilineP quotesP allowedCharP = lexeme $ fmap mconcat $ quotesP >> optional eol >> allowedCharP `manyTill` quotesP

multilineBasicStringP :: Parser Text
multilineBasicStringP = multilineP quotesP allowedCharP
  where
    quotesP = string "\"\"\""

    allowedCharP :: Parser Text
    allowedCharP = lineEndingBackslashP <|> escapeSequenceP <|> nonControlCharP <|> eol

    lineEndingBackslashP :: Parser Text
    lineEndingBackslashP = Text.empty <$ try (char '\\' >> eol >> space)

multilineLiteralStringP :: Parser Text
multilineLiteralStringP = multilineP quotesP allowedCharP
  where
    quotesP = string "'''"

    allowedCharP :: Parser Text
    allowedCharP = nonControlCharP <|> eol <|> Text.singleton <$> tab

-- Keys

bareKeyP :: Parser Text
bareKeyP = lexeme $ Text.pack <$> bareStrP
  where
    bareStrP :: Parser String
    bareStrP = some $ alphaNumChar <|> char '_' <|> char '-'

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

decimalP :: Parser Integer
decimalP = mkInteger <$> decimalStringP
  where
    decimalStringP   = fst <$> match (some digitChar >> many _digitsP)
    _digitsP         = try (char '_') >> some digitChar
    mkInteger        = textToInt . stripUnderscores
    textToInt        = fst . fromRight (error "Underscore parser has a bug") . TR.decimal
    stripUnderscores = Text.filter (/= '_')

integerP :: Parser Integer
integerP = lexeme (binary <|> octal <|> hexadecimal <|> decimal) <?> "integer"
  where
    decimal      = L.signed sc decimalP
    binary       = try (char '0' >> char 'b') >> L.binary
    octal        = try (char '0' >> char 'o') >> L.octal
    hexadecimal  = try (char '0' >> char 'x') >> L.hexadecimal

doubleP :: Parser Double
doubleP = lexeme (L.signed sc (num <|> inf <|> nan)) <?> "double"
  where
    num, inf, nan :: Parser Double
    num = L.float
    inf = 1 / 0 <$ string "inf"
    nan = 0 / 0 <$ string "nan"

boolP :: Parser Bool
boolP = False <$ text "false"
    <|> True  <$ text "true"
    <?> "bool"

dateTimeP :: Parser DateTime
dateTimeP = lexeme (try hoursP <|> dayLocalZoned) <?> "datetime"
  where
    -- dayLocalZoned can parse: only a local date, a local date with time, or
    -- a local date with a time and an offset
    dayLocalZoned :: Parser DateTime
    dayLocalZoned = do
      let makeLocal (Day day) (Hours hours) = Local $ LocalTime day hours
          makeLocal _         _             = error "Invalid arguments, unable to construct `Local`"
          makeZoned (Local localTime) mins = Zoned $ ZonedTime localTime (minutesToTimeZone mins)
          makeZoned _                 _    = error "Invalid arguments, unable to construct `Zoned`"
      day        <- try dayP
      maybeHours <- optional (try $ (char 'T' <|> char ' ') *> hoursP)
      case maybeHours of
        Nothing    -> return day
        Just hours -> do
          maybeOffset <- optional (try timeOffsetP)
          case maybeOffset of
            Nothing     -> return (makeLocal day hours)
            Just offset -> return (makeZoned (makeLocal day hours) offset)

    timeOffsetP :: Parser Int
    timeOffsetP = z <|> numOffset
      where
        z = 0 <$ char 'Z'
        numOffset = do
          sign    <- char '+' <|> char '-'
          hours   <- int2DigitsP
          _       <- char ':'
          minutes <- int2DigitsP
          let totalMinutes = hours * 60 + minutes
          if sign == '+'
             then return totalMinutes
             else return (negate totalMinutes)

    hoursP :: Parser DateTime
    hoursP = do
      hours   <- int2DigitsP
      _       <- char ':'
      minutes <- int2DigitsP
      _       <- char ':'
      seconds <- picoTruncated
      case makeTimeOfDayValid hours minutes seconds of
        Just time -> return (Hours time)
        Nothing   -> fail $ "Invalid time of day: " <> show hours <> ":" <> show minutes <> ":" <> show seconds

    dayP :: Parser DateTime
    dayP = do
      year  <- integer4DigitsP
      _     <- char '-'
      month <- int2DigitsP
      _     <- char '-'
      day   <- int2DigitsP
      case fromGregorianValid year month day of
        Just date -> return (Day date)
        Nothing   -> fail $ "Invalid date: " <> show year <> "-" <> show month <> "-" <> show day

    integer4DigitsP = (read :: String -> Integer) <$> count 4 digitChar
    int2DigitsP     = (read :: String -> Int)     <$> count 2 digitChar
    picoTruncated   = do
      let rdPico = read :: String -> Pico
      int <- count 2 digitChar
      frc <- optional (char '.' >> take 12 <$> some digitChar)
      case frc of
        Nothing   -> return (rdPico int)
        Just frc' -> return (rdPico $ int ++ "." ++ frc')

arrayP :: Parser [UValue]
arrayP = lexeme (between (char '[' *> sc) (char ']') elements) <?> "array"
  where
    elements :: Parser [UValue]
    elements = do v <- valueP -- Parse the first value to determine the type
                  sep <- optional spComma
                  vs <- case sep of
                          Nothing -> return []
                          Just _  -> (element v `sepEndBy` spComma) <* skipMany spComma
                  return (v:vs)
               <|> return []

    element :: UValue -> Parser UValue
    element = \case
                 UBool    _ -> UBool    <$> boolP
                 UDate    _ -> UDate    <$> dateTimeP
                 UDouble  _ -> UDouble  <$> try doubleP
                 UInteger _ -> UInteger <$> integerP
                 UText    _ -> UText    <$> textP
                 UArray   _ -> UArray   <$> arrayP

    spComma :: Parser ()
    spComma = char ',' *> sc

valueP :: Parser UValue
valueP = UBool    <$> boolP
     <|> UDate    <$> dateTimeP
     <|> UDouble  <$> try doubleP
     <|> UInteger <$> integerP
     <|> UText    <$> textP
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
    Left err   -> Left $ ParseException $ Text.pack $ errorBundlePretty err
    Right toml -> Right toml
