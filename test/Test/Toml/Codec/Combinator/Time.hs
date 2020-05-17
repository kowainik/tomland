module Test.Toml.Codec.Combinator.Time
    ( timeSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Toml.Codec.Combinator.Common (codecRoundtrip)

import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec.Combinator.Time as Toml


timeSpec :: Spec
timeSpec = describe "Combinator.Time: Roundtrip tests" $ do
    codecRoundtrip "ZonedTime " Toml.zonedTime Gen.genZonedTime
    codecRoundtrip "LocalTime " Toml.localTime Gen.genLocalTime
    codecRoundtrip "Day       " Toml.day       Gen.genDay
    codecRoundtrip "TimeOfDay " Toml.timeOfDay Gen.genTimeOfDay
