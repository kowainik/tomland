module Benchmark.Type
       ( Test(..)
       , test
       , testCodec
       ) where

import Data.Text (Text)
import Data.Time (ZonedTime)
import Data.Aeson.Types (FromJSON, withObject, parseJSON, (.:))

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
    , testBool   :: [Bool]
    , testToday  :: ZonedTime
    , testFruit  :: FruitInside
    , testSize   :: SizeInside
    }
    deriving Show

instance FromJSON Test where
    parseJSON = withObject "Test" $ \o -> Test
        <$> o .: "title"
        <*> o .: "atom"
        <*> o .: "cash"
        <*> o .: "words"
        <*> o .: "bool"
        <*> o .: "today"
        <*> o .: "fruit"
        <*> o .: "size"

data FruitInside = FruitInside
    { fiName        :: Text
    , fiDescription :: Text
    }
    deriving Show

instance FromJSON FruitInside where
    parseJSON = withObject "FruitInside" $ \o -> FruitInside
        <$> o .: "name"
        <*> o .: "description"

insideF :: TomlCodec FruitInside
insideF = FruitInside
    <$> Toml.text "name" .= fiName
    <*> Toml.text "description" .= fiDescription

newtype SizeInside = SizeInside
    { unSize :: [[Double]]
    }
    deriving Show

instance FromJSON SizeInside where
    parseJSON = withObject "SizeInside" $ \o -> SizeInside <$> o .: "dimensions"

insideS :: TomlCodec SizeInside
insideS = Toml.dimap unSize SizeInside $ Toml.arrayOf (Toml._Array Toml._Double) "dimensions"

testCodec :: TomlCodec Test
testCodec = Test
    <$> Toml.text "title" .= testTitle
    <*> Toml.double "atom" .= testAtom
    <*> Toml.bool "cash" .= testCash
    <*> Toml.arrayOf Toml._Text "words" .= testWords
    <*> Toml.arrayOf Toml._Bool "bool" .= testBool
    <*> Toml.zonedTime "today" .= testToday
    <*> Toml.table insideF "fruit" .= testFruit
    <*> Toml.table insideS "size" .= testSize
