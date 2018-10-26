module Benchmark.Type
       ( test
       , testCodec
       ) where

import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay, ZonedTime)

import Toml (TomlCodec, pretty, (.=))
import Toml.Parser (parse)

import qualified Data.Text.IO as TIO
import qualified Toml


test :: IO ()
test = do
    tomlFile <- TIO.readFile "./benchmark/benchmark.toml"
    TIO.putStrLn $ case Toml.decode testCodec tomlFile of
        Left msg    -> Toml.prettyException msg
        Right value -> Toml.encode testCodec value
    case parse tomlFile of
        Left (Toml.ParseException msg) -> TIO.putStrLn msg
        Right toml -> TIO.putStrLn $ case Toml.runTomlCodec testCodec toml of
            Left msg    -> Toml.prettyException msg
            Right value -> pretty $ Toml.execTomlCodec testCodec value

data Test = Test
    { testTitle  :: Text
    , testAtom   :: Double
    , testCash   :: Bool
    , testWords  :: [Text]
    , testDigits :: [Double]
    , testBool   :: [Bool]
    , testDate   :: [Day]
    , testToday  :: ZonedTime
    , testFruit  :: FruitInside
    , testSize   :: SizeInside
    }

data FruitInside = FruitInside
    { fiName        :: Text
    , fiDescription :: Text
    , fiHarvest     :: LocalTime
    , fiDue         :: TimeOfDay
    }

insideF :: TomlCodec FruitInside
insideF = FruitInside
    <$> Toml.text "name" .= fiName
    <*> Toml.text "description" .= fiDescription
    <*> Toml.localTime "harvest" .= fiHarvest
    <*> Toml.timeOfDay "due" .= fiDue

newtype SizeInside = SizeInside
    { unSize :: [[Double]]
    }

insideS :: TomlCodec SizeInside
insideS = Toml.dimap unSize SizeInside $ Toml.arrayOf (Toml._Array Toml._Double) "dimensions"

testCodec :: TomlCodec Test
testCodec = Test
    <$> Toml.text "title" .= testTitle
    <*> Toml.double "atom" .= testAtom
    <*> Toml.bool "cash" .= testCash
    <*> Toml.arrayOf Toml._Text "words" .= testWords
    <*> Toml.arrayOf Toml._Double "digits" .= testDigits
    <*> Toml.arrayOf Toml._Bool "bool" .= testBool
    <*> Toml.arrayOf Toml._Day "date" .= testDate
    <*> Toml.zonedTime "today" .= testToday
    <*> Toml.table insideF "fruit" .= testFruit
    <*> Toml.table insideS "size" .= testSize
