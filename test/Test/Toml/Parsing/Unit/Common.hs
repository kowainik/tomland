{-# LANGUAGE FlexibleContexts #-}

module Test.Toml.Parsing.Unit.Common
       ( parseX
       , failOn
       , parseArray
       , parseBool
       , parseDouble
       , parseInteger
       , parseKey
       , parseText
       , parseToml
       , parseDateTime
       , arrayFailOn
       , boolFailOn
       , integerFailOn
       , dateTimeFailOn
       , doubleFailOn
       , textFailOn
       , tomlFailOn
       , quoteWith
       , squote
       , dquote
       , squote3
       , dquote3
       , makeKey
       , makeOffset
       , makeZoned
       , int1
       , int2
       , int3
       , int4
       , offset710
       , offset0
       , day1
       , day2
       , hours1
       ) where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay (..), TimeZone, ZonedTime (..), fromGregorian,
                  minutesToTimeZone)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Test.Tasty.Hspec (Expectation)
import Text.Megaparsec (Parsec, ShowErrorComponent, Stream, parse)

import Toml.Parser.Item (tomlP)
import Toml.Parser.Key (keyP)
import Toml.Parser.String (textP)
import Toml.Parser.Validate (validateItems)
import Toml.Parser.Value (arrayP, boolP, dateTimeP, doubleP, integerP)
import Toml.PrefixTree (Key (..), Piece (..))
import Toml.Type (TOML (..), UValue (..))

import qualified Data.List.NonEmpty as NE

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

parseX
    :: (ShowErrorComponent e, Stream s, Show a, Eq a)
    => Parsec e s a -> s -> a -> Expectation
parseX p given expected = parse p "" given `shouldParse` expected

failOn :: Show a => Parsec e s a -> s -> Expectation
failOn p given = parse p "" `shouldFailOn` given

parseArray :: Text -> [UValue] -> Expectation
parseArray = parseX arrayP

parseBool :: Text -> Bool -> Expectation
parseBool = parseX boolP

parseDateTime :: Text -> UValue -> Expectation
parseDateTime = parseX dateTimeP

parseDouble :: Text -> Double -> Expectation
parseDouble = parseX doubleP

parseInteger :: Text -> Integer -> Expectation
parseInteger = parseX integerP

parseKey :: Text -> Key -> Expectation
parseKey = parseX keyP

parseText :: Text -> Text -> Expectation
parseText = parseX textP

parseToml :: Text -> TOML -> Expectation
parseToml test toml = parseX (validateItems <$> tomlP) test (Right toml)

arrayFailOn
  , boolFailOn
  , dateTimeFailOn
  , doubleFailOn
  , integerFailOn
  , textFailOn
  , tomlFailOn :: Text -> Expectation
arrayFailOn     = failOn arrayP
boolFailOn      = failOn boolP
dateTimeFailOn  = failOn dateTimeP
doubleFailOn    = failOn doubleP
integerFailOn   = failOn integerP
textFailOn      = failOn textP
tomlFailOn      = failOn tomlP

-- Surround given text with quotes.
quoteWith :: Text -> Text -> Text
quoteWith q t = q <> t <> q
squote, dquote, squote3, dquote3 :: Text -> Text
squote = quoteWith "'"
dquote = quoteWith "\""
squote3 = quoteWith "'''"
dquote3 = quoteWith "\"\"\""

-- UValue Util

makeZoned :: Day -> TimeOfDay -> TimeZone -> UValue
makeZoned d h offset = UZoned $ ZonedTime (LocalTime d h) offset

makeOffset :: Int -> Int -> TimeZone
makeOffset hours mins = minutesToTimeZone (hours * 60 + mins * signum hours)

makeKey :: [Text] -> Key
makeKey = Key . NE.fromList . map Piece

-- Test Data

int1, int2, int3, int4 :: UValue
int1 = UInteger 1
int2 = UInteger 2
int3 = UInteger 3
int4 = UInteger 4

offset0, offset710 :: TimeZone
offset0 = makeOffset 0 0
offset710 = makeOffset 7 10

day1, day2 :: Day
day1 = fromGregorian 1979 5 27  -- 1979-05-27
day2 = fromGregorian 1920 12 10 -- 1920-12-10

hours1 :: TimeOfDay
hours1 = TimeOfDay 7 32 0  -- 07:32:00
