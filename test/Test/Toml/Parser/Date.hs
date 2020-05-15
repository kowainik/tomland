module Test.Toml.Parser.Date
    ( dateSpecs
    ) where

import Data.Time (LocalTime (..), TimeOfDay (..))
import Test.Hspec (Spec, describe, it)

import Test.Toml.Parser.Common (dateTimeFailOn, day1, hours1, makeOffset, makeZoned, offset0,
                                offset710, parseDateTime)
import Toml.Type (UValue (..))


dateSpecs :: Spec
dateSpecs = describe "dateTimeP" $ do
    it "can parse a date-time with an offset" $ do
        parseDateTime "1979-05-27T07:32:00Z" $
            makeZoned day1 hours1 offset0
        parseDateTime "1979-05-27T00:32:00+07:10" $
            makeZoned day1 (TimeOfDay 0 32 0) offset710
        parseDateTime "1979-05-27T00:32:00.999999-07:25" $
            makeZoned day1 (TimeOfDay 0 32 0.999999) (makeOffset (-7) 25)
    it "can parse a date-time with an offset when the T delimiter is replaced with a space" $
        parseDateTime "1979-05-27 07:32:00Z" $
            makeZoned day1 hours1 offset0
    it "can parse a date-time without an offset" $ do
        parseDateTime "1979-05-27T17:32:00"
            (ULocal $ LocalTime day1 (TimeOfDay 17 32 0))
        parseDateTime "1979-05-27T00:32:00.999999"
            (ULocal $ LocalTime day1 (TimeOfDay 0 32 0.999999))
    it "can parse a local date" $
        parseDateTime "1979-05-27" (UDay day1)
    it "can parse a local time" $ do
        parseDateTime "07:32:00"        (UHours hours1)
        parseDateTime "00:32:00.999999" (UHours $ TimeOfDay 0 32 0.999999)
    it "truncates the additional precision after picoseconds in the fractional seconds" $
        parseDateTime "00:32:00.99999999999199"
            (UHours $ TimeOfDay 0 32 0.999999999991)
    it "fails if the date is not valid" $ do
        dateTimeFailOn "1920-15-12"
        dateTimeFailOn "1920-12-40"
    it "fails if the date does not have the form: 'yyyy-mm-dd'" $ do
        dateTimeFailOn "1920-01-1"
        dateTimeFailOn "1920-1-01"
        dateTimeFailOn "920-01-01"
        dateTimeFailOn "1920/10/01"
    it "fails if the time is not valid" $ do
        dateTimeFailOn "25:10:10"
        dateTimeFailOn "10:70:10"
        dateTimeFailOn "10:10:70"
    it "fails if the time does not have the form: 'hh:mm:ss'" $ do
        dateTimeFailOn "1:12:12"
        dateTimeFailOn "12:1:12"
        dateTimeFailOn "12:12:1"
        dateTimeFailOn "12-12-12"
    it "fails if the offset does not have any of the forms: 'Z', '+hh:mm', '-hh:mm'" $ do
        parseDateTime "1979-05-27T00:32:00X"
            (ULocal $ LocalTime day1 (TimeOfDay 0 32 0))
        parseDateTime "1979-05-27T00:32:00+07:1"
            (ULocal $ LocalTime day1 (TimeOfDay 0 32 0))
        parseDateTime "1979-05-27T00:32:00+7:01"
            (ULocal $ LocalTime day1 (TimeOfDay 0 32 0))
        parseDateTime "1979-05-27T00:32:0007:00"
            (ULocal $ LocalTime day1 (TimeOfDay 0 32 0))
