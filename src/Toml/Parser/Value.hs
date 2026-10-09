{- |
Module                  : Toml.Parser.Value
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Parser for 'UValue'.
-}

module Toml.Parser.Value
       ( arrayP
       , boolP
       , dateTimeP
       , doubleP
       , integerP
       , valueP
       , anyValueP
       ) where

import Control.Applicative (Alternative (..))
import Control.Applicative.Combinators (between, count, option, optional, sepBy1)
import Control.Monad (when)
import Data.Fixed (Pico)
import Data.String (fromString)
import Data.Time (Day, LocalTime (..), TimeOfDay, ZonedTime (..), fromGregorianValid,
                  makeTimeOfDayValid, minutesToTimeZone)

import Text.Megaparsec (parseMaybe)
import Text.Read (readMaybe)

import Toml.Parser.Core (Parser, binDigitChar, binary, char, digitChar, hexDigitChar, hexadecimal,
                         lexeme, octDigitChar, octal, scn, signed, string, text, try, (<?>))
import Toml.Parser.String (textP)
import Toml.Type (AnyValue, UValue (..), typeCheck)


{- | Parser for decimal digits with underscores between them, e.g. @1_000@.
Returns the digits without underscores.
-}
digitsP :: Parser String
digitsP = concat <$> sepBy1 (some digitChar) (char '_')

-- | Fails if the given digits have a leading zero.
checkLeadingZero :: String -> Parser ()
checkLeadingZero = \case
    '0' : _ : _ -> fail "Leading zero."
    _           -> pure ()

-- | Parser for decimal 'Integer': included parsing of underscore.
decimalP :: Parser Integer
decimalP = do
    ds <- digitsP
    checkLeadingZero ds
    maybe (fail "Not an integer") pure (readMaybe ds)

-- | Parser for hexadecimal, octal and binary numbers : included parsing
numberP :: Parser Integer -> Parser Char -> String -> Parser Integer
numberP parseInteger parseDigit errorMessage = more
  where
    more :: Parser Integer
    more = check =<< intValueMaybe . concat <$> sepBy1 (some parseDigit) (char '_')

    intValueMaybe :: String -> Maybe Integer
    intValueMaybe = parseMaybe parseInteger . fromString

    check :: Maybe Integer -> Parser Integer
    check = maybe (fail errorMessage) pure

-- | Parser for 'Integer' value.
integerP :: Parser Integer
integerP = lexeme (bin <|> oct <|> hex <|> dec) <?> "integer"
  where
    bin, oct, hex, dec :: Parser Integer
    bin = try (char '0' *> char 'b') *> binaryP      <?> "bin"
    oct = try (char '0' *> char 'o') *> octalP       <?> "oct"
    hex = try (char '0' *> char 'x') *> hexadecimalP <?> "hex"
    dec = signed (pure ()) decimalP                  <?> "dec"
    binaryP = numberP binary binDigitChar "Invalid binary number"
    octalP  = numberP octal octDigitChar  "Invalid ocatl number"
    hexadecimalP = numberP hexadecimal hexDigitChar "Invalid hexadecimal number"

-- | Parser for 'Double' value.
doubleP :: Parser Double
doubleP = lexeme (signed (pure ()) (num <|> inf <|> nan)) <?> "double"
  where
    num, inf, nan :: Parser Double
    num = floatP
    inf = 1 / 0 <$ string "inf"
    nan = 0 / 0 <$ string "nan"

-- | Parser for 'Double' numbers. Used in 'doubleP'.
floatP :: Parser Double
floatP = do
    intPart <- digitsP
    checkLeadingZero intPart
    rest <- expo <|> dot
    maybe (fail "Not a float") pure $ readMaybe (intPart ++ rest)
  where
    dot, expo :: Parser String
    dot = mconcat [pure <$> char '.', digitsP, option "" expo]
    expo = mconcat
        [ pure <$> (char 'e' <|> char 'E')
        , pure <$> option '+' (char '+' <|> char '-')
        , digitsP
        ]

-- | Parser for 'Bool' value.
boolP :: Parser Bool
boolP = False <$ text "false"
    <|> True  <$ text "true"
    <?> "bool"

-- | Parser for datetime values.
dateTimeP :: Parser UValue
dateTimeP = lexeme (try (UHours <$> hoursP) <|> dayLocalZoned) <?> "datetime"

-- dayLocalZoned can parse: only a local date, a local date with time, or
-- a local date with a time and an offset
dayLocalZoned :: Parser UValue
dayLocalZoned = do
    day        <- try dayP
    maybeHours <- optional (try $ (char 'T' <|> char 't' <|> char ' ') *> hoursP)
    case maybeHours of
        Nothing    -> pure $ UDay day
        Just hours -> do
            maybeOffset <- optional (try timeOffsetP)
            let localTime = LocalTime day hours
            pure $ case maybeOffset of
                Nothing     -> ULocal localTime
                Just offset -> UZoned $ ZonedTime localTime (minutesToTimeZone offset)

-- | Parser for time-zone offset
timeOffsetP :: Parser Int
timeOffsetP = z <|> numOffset
  where
    z :: Parser Int
    z = 0 <$ (char 'Z' <|> char 'z')

    numOffset :: Parser Int
    numOffset = do
        sign    <- char '+' <|> char '-'
        hours   <- int2DigitsP
        _       <- char ':'
        minutes <- int2DigitsP
        when (hours > 23 || minutes > 59) $ fail $
            "Invalid time offset: " <> show hours <> ":" <> show minutes
        let totalMinutes = hours * 60 + minutes
        pure $ if sign == '+'
            then totalMinutes
            else negate totalMinutes

{- | Parser for offset in day. Seconds may be omitted, in which case @:00@ is
assumed.
-}
hoursP :: Parser TimeOfDay
hoursP = do
    hours   <- int2DigitsP
    _ <- char ':'
    minutes <- int2DigitsP
    seconds <- option 0 (char ':' *> picoTruncated)
    case makeTimeOfDayValid hours minutes seconds of
        Just time -> pure time
        Nothing   -> fail $
           "Invalid time of day: " <> show hours <> ":" <> show minutes <> ":" <> show seconds

-- | Parser for 'Day'.
dayP :: Parser Day
dayP = do
    year  <- yearP
    _     <- char '-'
    month <- int2DigitsP
    _     <- char '-'
    day   <- int2DigitsP
    case fromGregorianValid year month day of
        Just date -> pure date
        Nothing   -> fail $
            "Invalid date: " <> show year <> "-" <> show month <> "-" <> show day

-- | Parser for exactly 4 integer digits.
yearP :: Parser Integer
yearP = read <$> count 4 digitChar

-- | Parser for exactly two digits. Used to parse months or hours.
int2DigitsP :: Parser Int
int2DigitsP = read <$> count 2 digitChar

-- | Parser for pico-chu.
picoTruncated :: Parser Pico
picoTruncated = do
    int <- count 2 digitChar
    frc <- optional $ char '.' *> (take 12 <$> some digitChar)
    pure $ read $ case frc of
        Nothing   -> int
        Just frc' -> int ++ "." ++ frc'

{- | Parser for array of values. Elements may have different types. Newlines
and comments are allowed between the elements. A single trailing comma is
allowed after the last element.
-}
arrayP :: Parser [UValue]
arrayP = lexeme (between (char '[' *> scn) (char ']') elements) <?> "array"
  where
    elements :: Parser [UValue]
    elements = option [] $ do -- Zero or more elements
        v  <- valueP <* scn
        vs <- many (try (spComma *> valueP) <* scn)
        _  <- optional spComma
        pure (v:vs)

    spComma :: Parser ()
    spComma = char ',' *> scn

-- | Parser for 'UValue'.
valueP :: Parser UValue
valueP = UText    <$> textP
     <|> UBool    <$> boolP
     <|> UArray   <$> arrayP
     <|> dateTimeP
     <|> UDouble  <$> try doubleP
     <|> UInteger <$> integerP

-- | Uses 'valueP' and typechecks it.
anyValueP :: Parser AnyValue
anyValueP = typeCheck <$> valueP >>= \case
    Left err -> fail $ show err
    Right v  -> pure v
