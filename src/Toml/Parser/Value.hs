module Toml.Parser.Value
       ( arrayP
       , boolP
       , dateTimeP
       , doubleP
       , integerP
       , keyP
       , tableNameP
       , tableArrayNameP
       , textP
       , valueP
       , anyValueP
       ) where

import Control.Applicative (Alternative (some, (<|>)))
import Control.Applicative.Combinators (between, count, manyTill, option, optional, sepBy1,
                                        sepEndBy, skipMany)

import Data.Char (chr, isControl)
import Data.Fixed (Pico)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay, ZonedTime (..), fromGregorianValid,
                  makeTimeOfDayValid, minutesToTimeZone)
import Text.Read (readMaybe)

import Toml.Parser.Core (Parser, alphaNumChar, anySingle, binary, char, digitChar, eol,
                         hexDigitChar, hexadecimal, lexeme, octal, satisfy, sc, signed, space,
                         string, tab, text, try, (<?>))
import Toml.PrefixTree (Key (..), Piece (..))
import Toml.Type (AnyValue, UValue (..), typeCheck)

import qualified Control.Applicative.Combinators.NonEmpty as NC
import qualified Data.Text as Text

-- | Parser for TOML text.
textP :: Parser Text
textP = multilineBasicStringP
    <|> multilineLiteralStringP
    <|> literalStringP
    <|> basicStringP
    <?> "text"

-- | Parse a non-control character (control character is a non-printing
-- character of the Latin-1 subset of Unicode).
nonControlCharP :: Parser Text
nonControlCharP = Text.singleton <$> satisfy (not . isControl) <?> "non-control char"

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
    hexUnicodeP n = count n hexDigitChar >>= \x -> case toUnicode $ hexToInt x of
        Just c  -> pure (Text.singleton c)
        Nothing -> fail $ "Invalid unicode character: \\"
            <> (if n == 4 then "u" else "U")
            <> x
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
basicStringP = lexeme $ mconcat
    <$> (char '"' *> (escapeSequenceP <|> nonControlCharP) `manyTill` char '"')


literalStringP :: Parser Text
literalStringP = lexeme $ Text.pack <$> (char '\'' *> nonEolCharP `manyTill` char '\'')
  where
    nonEolCharP :: Parser Char
    nonEolCharP = satisfy (\c -> c /= '\n' && c /= '\r')


multilineP :: Parser Text -> Parser Text -> Parser Text
multilineP quotesP allowedCharP = lexeme $ fmap mconcat $ quotesP
                               >> optional eol
                               >> allowedCharP `manyTill` quotesP


multilineBasicStringP :: Parser Text
multilineBasicStringP = multilineP quotesP allowedCharP
  where
    quotesP :: Parser Text
    quotesP = string "\"\"\""

    allowedCharP :: Parser Text
    allowedCharP = lineEndingBackslashP <|> escapeSequenceP <|> nonControlCharP <|> eol

    lineEndingBackslashP :: Parser Text
    lineEndingBackslashP = Text.empty <$ try (char '\\' >> eol >> space)


multilineLiteralStringP :: Parser Text
multilineLiteralStringP = multilineP quotesP allowedCharP
  where
    quotesP :: Parser Text
    quotesP = string "'''"

    allowedCharP :: Parser Text
    allowedCharP = nonControlCharP <|> eol <|> Text.singleton <$> tab

-- Keys

bareKeyP :: Parser Text
bareKeyP = lexeme $ Text.pack <$> bareStrP
  where
    bareStrP :: Parser String
    bareStrP = some $ alphaNumChar <|> char '_' <|> char '-'


keyComponentP :: Parser Piece
keyComponentP = Piece <$>
    (bareKeyP <|> (quote "\"" <$> basicStringP) <|> (quote "'" <$> literalStringP))
  where
    -- adds " or ' to both sides
    quote :: Text -> Text -> Text
    quote q t = q <> t <> q


keyP :: Parser Key
keyP = Key <$> NC.sepBy1 keyComponentP (char '.')


tableNameP :: Parser Key
tableNameP = between (text "[") (text "]") keyP


tableArrayNameP :: Parser Key
tableArrayNameP = between (text "[[") (text "]]") keyP


-- Values

decimalP :: Parser Integer
decimalP = zero <|> more
  where
    zero = 0 <$ char '0'
    more = check =<< readMaybe . concat <$> sepBy1 (some digitChar) (char '_')
    check = maybe (fail "Not an integer") return


integerP :: Parser Integer
integerP = lexeme (bin <|> oct <|> hex <|> dec) <?> "integer"
  where
    bin = try (char '0' >> char 'b') >> binary
    oct = try (char '0' >> char 'o') >> octal
    hex = try (char '0' >> char 'x') >> hexadecimal
    dec = signed sc decimalP


doubleP :: Parser Double
doubleP = lexeme (signed sc (num <|> inf <|> nan)) <?> "double"
  where
    num, inf, nan :: Parser Double
    num = floatP
    inf = 1 / 0 <$ string "inf"
    nan = 0 / 0 <$ string "nan"

floatP :: Parser Double
floatP = check . readMaybe =<< mconcat [ digits, expo <|> dot ]
  where
    check = maybe (fail "Not a float") return

    digits, dot, expo :: Parser String
    digits = concat <$> sepBy1 (some digitChar) (char '_')
    dot = mconcat [pure <$> char '.', digits, option "" expo]
    expo = mconcat
             [ pure <$> (char 'e' <|> char 'E')
             , pure <$> option '+' (char '+' <|> char '-')
             , digits
             ]

boolP :: Parser Bool
boolP = False <$ text "false"
    <|> True  <$ text "true"
    <?> "bool"


dateTimeP :: Parser UValue
dateTimeP = lexeme (try (UHours <$> hoursP) <|> dayLocalZoned) <?> "datetime"

-- dayLocalZoned can parse: only a local date, a local date with time, or
-- a local date with a time and an offset
dayLocalZoned :: Parser UValue
dayLocalZoned = do
    day        <- try dayP
    maybeHours <- optional (try $ (char 'T' <|> char ' ') *> hoursP)
    case maybeHours of
        Nothing    -> pure $ UDay day
        Just hours -> do
            maybeOffset <- optional (try timeOffsetP)
            let localTime = LocalTime day hours
            pure $ case maybeOffset of
                Nothing     -> ULocal localTime
                Just offset -> UZoned $ ZonedTime localTime (minutesToTimeZone offset)

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
      return $ if sign == '+'
                then totalMinutes
                else negate totalMinutes

hoursP :: Parser TimeOfDay
hoursP = do
    hours   <- int2DigitsP
    _ <- char ':'
    minutes <- int2DigitsP
    _ <- char ':'
    seconds <- picoTruncated
    case makeTimeOfDayValid hours minutes seconds of
      Just time -> pure time
      Nothing   -> fail $
         "Invalid time of day: " <> show hours <> ":" <> show minutes <> ":" <> show seconds

dayP :: Parser Day
dayP = do
    year  <- integer4DigitsP
    _     <- char '-'
    month <- int2DigitsP
    _     <- char '-'
    day   <- int2DigitsP
    case fromGregorianValid year month day of
      Just date -> pure date
      Nothing   -> fail $ "Invalid date: " <> show year <> "-" <> show month <> "-" <> show day

integer4DigitsP :: Parser Integer
integer4DigitsP = read <$> count 4 digitChar

int2DigitsP :: Parser Int
int2DigitsP = read <$> count 2 digitChar

picoTruncated :: Parser Pico
picoTruncated = do
    int <- count 2 digitChar
    frc <- optional (char '.' >> take 12 <$> some digitChar)
    pure $ read $ case frc of
        Nothing   -> int
        Just frc' -> int ++ "." ++ frc'


arrayP :: Parser [UValue]
arrayP = lexeme (between (char '[' *> sc) (char ']') elements) <?> "array"
  where
    elements :: Parser [UValue]
    elements = option [] $ do -- Zero or more elements
        v   <- valueP -- Parse the first value to determine the type
        sep <- optional spComma
        vs  <- case sep of
                 Nothing -> pure []
                 Just _  -> (element v `sepEndBy` spComma) <* skipMany spComma
        pure (v:vs)

    element :: UValue -> Parser UValue
    element = \case
        UBool    _ -> UBool    <$> boolP
        UZoned   _ -> dayLocalZoned
        ULocal   _ -> dayLocalZoned
        UDay     _ -> UDay     <$> dayP
        UHours   _ -> UHours   <$> hoursP
        UDouble  _ -> UDouble  <$> try doubleP
        UInteger _ -> UInteger <$> integerP
        UText    _ -> UText    <$> textP
        UArray   _ -> UArray   <$> arrayP

    spComma :: Parser ()
    spComma = char ',' *> sc


valueP :: Parser UValue
valueP = UText    <$> textP
     <|> UBool    <$> boolP
     <|> UArray   <$> arrayP
     <|> dateTimeP
     <|> UDouble  <$> try doubleP
     <|> UInteger <$> integerP

anyValueP :: Parser AnyValue
anyValueP = typeCheck <$> valueP >>= \case
    Left err -> fail $ show err
    Right v  -> return v
