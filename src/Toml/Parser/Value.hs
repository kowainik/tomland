module Toml.Parser.Value
       ( arrayP
       , boolP
       , dateTimeP
       , doubleP
       , integerP
       , keyP
       , tableNameP
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
import Data.Time (LocalTime (..), ZonedTime (..), fromGregorianValid, makeTimeOfDayValid,
                  minutesToTimeZone)
import Text.Read (readMaybe)

import Toml.Parser.Core (Parser, alphaNumChar, anySingle, binary, char, digitChar, eol, float,
                         hexDigitChar, hexadecimal, lexeme, octal, satisfy, sc, signed, space,
                         string, tab, text, try, (<?>))
import Toml.PrefixTree (Key (..), Piece (..))
import Toml.Type (AnyValue, DateTime (..), UValue (..), typeCheck)

import qualified Control.Applicative.Combinators.NonEmpty as NC
import qualified Data.Text as Text

textP :: Parser Text
textP = multilineBasicStringP
     <|> multilineLiteralStringP
     <|> literalStringP
     <|> basicStringP
     <?> "text"


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
                          Nothing -> fail $ "Invalid unicode character: "
                                          <> "\\"
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


keyComponentP :: Parser Piece
keyComponentP = Piece <$> (
                  bareKeyP <|> (quote "\"" <$> basicStringP) <|> (quote "'" <$> literalStringP)
                )
  where
    -- adds " or ' to both sides
    quote q t = q <> t <> q


keyP :: Parser Key
keyP = Key <$> NC.sepBy1 keyComponentP (char '.')


tableNameP :: Parser Key
tableNameP = between (text "[") (text "]") keyP


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
    num = float
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
          makeZoned (Local localTime) mins  = Zoned $ ZonedTime localTime (minutesToTimeZone mins)
          makeZoned _                 _     = error "Invalid arguments, unable to construct `Zoned`"
      day        <- try dayP
      maybeHours <- optional (try $ (char 'T' <|> char ' ') *> hoursP)
      case maybeHours of
        Nothing    -> return day
        Just hours -> do
          maybeOffset <- optional (try timeOffsetP)
          return $ case maybeOffset of
                    Nothing     -> makeLocal day hours
                    Just offset -> makeZoned (makeLocal day hours) offset

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

    hoursP :: Parser DateTime
    hoursP = do
      hours   <- int2DigitsP
      _       <- char ':'
      minutes <- int2DigitsP
      _       <- char ':'
      seconds <- picoTruncated
      case makeTimeOfDayValid hours minutes seconds of
        Just time -> return (Hours time)
        Nothing   -> fail $ "Invalid time of day: "
                          <> show hours
                          <> ":"
                          <> show minutes
                          <> ":" <> show seconds

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
    elements = option [] $ do -- Zero or more elements
                 v   <- valueP -- Parse the first value to determine the type
                 sep <- optional spComma
                 vs  <- case sep of
                          Nothing -> pure []
                          Just _  -> (element v `sepEndBy` spComma) <* skipMany spComma
                 return (v:vs)

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
valueP = UText    <$> textP
     <|> UBool    <$> boolP
     <|> UArray   <$> arrayP
     <|> UDate    <$> dateTimeP
     <|> UDouble  <$> try doubleP
     <|> UInteger <$> integerP

anyValueP :: Parser AnyValue
anyValueP = typeCheck <$> valueP >>= \case
    Left err -> fail $ show err
    Right v  -> return v
