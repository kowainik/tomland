module Benchmark.TomlData
       (
         test
       , codecToml
       , tomlData
       ) where

import Data.Text (Text)
-- import Data.Time (LocalTime (..), UTCTime (..), fromGregorian, timeToTimeOfDay, utc, utcToZonedTime)

import Toml (TomlCodec, pretty, (.=))
import Toml.Edsl (mkToml, table, (=:))
import Toml.Type (TOML (..), Value (..))

import qualified Data.Text.IO as IO
import qualified Toml


test :: IO ()
test = do
    tomlFile <- IO.readFile "./benchmark/benchmark.toml"
    IO.putStrLn $ case Toml.decode codecToml tomlFile of
        Left msg   -> Toml.prettyException msg
        Right hask -> Toml.encode codecToml hask
    IO.putStrLn $ case Toml.runTomlCodec codecToml tomlData of
        Left msg   -> Toml.prettyException msg
        Right hask -> pretty $ Toml.execTomlCodec codecToml hask

data Test = Test
    { title  :: Text
    , atom   :: Double
    , cash   :: Bool
    , chars  :: [Text]
    , digits :: [Double]
    , bool   :: [Bool]
    , fruit  :: FruitInside
    , size   :: SizeInside
    }

newtype FruitInside = FruitInside
    { unFruit :: Text }

insideF :: TomlCodec FruitInside
insideF = Toml.dimap unFruit FruitInside $ Toml.text "name"

newtype SizeInside = SizeInside
    { unSize :: [Double] }

insideS :: TomlCodec SizeInside
insideS = Toml.dimap unSize SizeInside $ Toml.arrayOf Toml._Double "length"

codecToml :: TomlCodec Test
codecToml = Test
    <$> Toml.text "title" .= title
    <*> Toml.double "atom" .= atom
    <*> Toml.bool "cash" .= cash
    <*> Toml.arrayOf Toml._Text "chars" .= chars
    <*> Toml.arrayOf Toml._Double "digits" .= digits
    <*> Toml.arrayOf Toml._Bool "bool" .= bool
    <*> Toml.table insideF "fruit" .= fruit
    <*> Toml.table insideS  "size" .= size

tomlData :: TOML
tomlData = mkToml $ do
    "title" =: "TOML Example"
    "atom" =: Double 6.626e-34
    "cash" =: Bool True
    "chars" =: Array ["a", "b", "?", "!", "`", "-", "  d", "\t", "\\", "\"" ]
    "digits" =: Array [Double 1.0, Double 2.0, Double (1/0), Double (-1/0), Double (0/0), Double (-0/0), Double 1.0]
    "bool" =: Array [Bool False, Bool True, Bool False]
    -- "date" =: Array [Date $ Day (fromGregorian 1979 05 27)]
    -- "today" =: (Date $ Zoned $ utcToZonedTime utc $
    --     UTCTime (fromGregorian 1979 05 27) (7 * 3600 + 32 * 60))

    table "fruit" $
        "name" =: "apple"
        -- "harvestDate" =: (Date $ Local $ LocalTime (fromGregorian 1979 05 27) $
        --     timeToTimeOfDay (32 * 60))
        -- "due" =: (Date $ Hours $ timeToTimeOfDay (7 * 3600 + 32 * 60))

    table "size" $
        "length" =: Array [Double (-455967.93489327864), Double 6.626e-34, Double 696332.8128600451]
        -- "colorsAndWeights" =: Array ["\\U5D71e37f", "\\Ud4dB3c0F", "\\UFCf862b5", "\\U141bdcFA", "\\U8fE68fc6"]
