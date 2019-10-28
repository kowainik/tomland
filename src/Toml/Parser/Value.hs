{- |
Copyright: (c) 2018-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

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
import Control.Applicative.Combinators (between, count, option, optional, sepBy1, sepEndBy,
                                        skipMany)
import Data.Fixed (Pico)
import Data.Semigroup ((<>))
import Data.Time (Day, LocalTime (..), TimeOfDay, ZonedTime (..), fromGregorianValid,
                  makeTimeOfDayValid, minutesToTimeZone)
import Text.Read (readMaybe)

import Toml.Parser.Core (Parser, binary, char, digitChar, hexadecimal, lexeme, octal, sc, signed,
                         string, text, try, (<?>))
import Toml.Parser.String (textP)
import Toml.Type (AnyValue, UValue (..), typeCheck)


-- | Parser for decimap 'Integer': included parsing of underscore.
decimalP :: Parser Integer
decimalP = zero <|> more
  where
    zero, more :: Parser Integer
    zero  = 0 <$ char '0'
    more  = check =<< readMaybe . concat <$> sepBy1 (some digitChar) (char '_')

    check :: Maybe Integer -> Parser Integer
    check = maybe (fail "Not an integer") pure

-- | Parser for 'Integer' value.
integerP :: Parser Integer
integerP = lexeme (bin <|> oct <|> hex <|> dec) <?> "integer"
  where
    bin, oct, hex, dec :: Parser Integer
    bin = try (char '0' *> char 'b') *> binary      <?> "bin"
    oct = try (char '0' *> char 'o') *> octal       <?> "oct"
    hex = try (char '0' *> char 'x') *> hexadecimal <?> "hex"
    dec = signed sc decimalP                        <?> "dec"

-- | Parser for 'Double' value.
doubleP :: Parser Double
doubleP = lexeme (signed sc (num <|> inf <|> nan)) <?> "double"
  where
    num, inf, nan :: Parser Double
    num = floatP
    inf = 1 / 0 <$ string "inf"
    nan = 0 / 0 <$ string "nan"

-- | Parser for 'Double' numbers. Used in 'doubleP'.
floatP :: Parser Double
floatP = check . readMaybe =<< mconcat [ digits, expo <|> dot ]
  where
    check :: Maybe Double -> Parser Double
    check = maybe (fail "Not a float") return

    digits, dot, expo :: Parser String
    digits = concat <$> sepBy1 (some digitChar) (char '_')
    dot = mconcat [pure <$> char '.', digits, option "" expo]
    expo = mconcat
        [ pure <$> (char 'e' <|> char 'E')
        , pure <$> option '+' (char '+' <|> char '-')
        , digits
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
    maybeHours <- optional (try $ (char 'T' <|> char ' ') *> hoursP)
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
    z = 0 <$ char 'Z'

    numOffset :: Parser Int
    numOffset = do
        sign    <- char '+' <|> char '-'
        hours   <- int2DigitsP
        _       <- char ':'
        minutes <- int2DigitsP
        let totalMinutes = hours * 60 + minutes
        pure $ if sign == '+'
            then totalMinutes
            else negate totalMinutes

-- | Parser for offset in day.
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

{- | Parser for array of values. This parser tries to parse first element of
array, pattern-matches on this element and uses parser according to this first
element. This allows to prevent parsing of heterogeneous arrays.
-}
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
