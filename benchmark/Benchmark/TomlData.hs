module Benchmark.TomlData
       ( test
       , codecToml
       , tomlData
       ) where

import Data.Text (Text)
import Control.Arrow ((>>>))

import Toml (TomlCodec, pretty, (.=), (<!>))
import Toml.Edsl (mkToml, table, (=:))
import Toml.Type (TOML (..), Value (..))

import qualified Data.Text.IO as TIO
import qualified Toml


test :: IO ()
test = do
    tomlFile <- TIO.readFile "./benchmark/benchmark.toml"
    TIO.putStrLn $ case Toml.decode codecToml tomlFile of
        Left msg   -> Toml.prettyException msg
        Right hask -> Toml.encode codecToml hask
    TIO.putStrLn $ case Toml.runTomlCodec codecToml tomlData of
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

data FruitInside = FruitInside
    { name :: Text
    , description :: Text
    }

insideF :: TomlCodec FruitInside
insideF = FruitInside <$> (Toml.text "name" .= name) <*> (Toml.text "description" .= description)

newtype SizeInside = SizeInside
    { unSize :: [Either [Text] [Double]]}

insideS :: TomlCodec SizeInside
insideS = Toml.dimap unSize SizeInside $ eitherTD "distance"

codecToml :: TomlCodec Test
codecToml = Test
    <$> Toml.text "title" .= title
    <*> Toml.double "atom" .= atom
    <*> Toml.bool "cash" .= cash
    <*> Toml.arrayOf Toml._Text "chars" .= chars
    <*> Toml.arrayOf Toml._Double "digits" .= digits
    <*> Toml.arrayOf Toml._Bool "bool" .= bool
    <*> Toml.table insideF "fruit" .= fruit
    <*> Toml.table insideS "size" .= size

eitherTD :: Toml.Key -> TomlCodec [Either [Text] [Double]]
eitherTD = Toml.arrayOf (Toml._Left >>> (Toml._Array Toml._Text))
         <!> Toml.arrayOf (Toml._Right >>> (Toml._Array Toml._Double))

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

    table "fruit" $ do
        "name" =: "apple"
        "description" =: "An apple is a sweet, edible fruit produced by an apple tree (Malus pumila). Apple trees are cultivated worldwide, and are the most widely grown species in the genus Malus. The tree originated in Central Asia, where its wild ancestor, Malus sieversii, is still found today."
        -- "harvestDate" =: (Date $ Local $ LocalTime (fromGregorian 1979 05 27) $
        --     timeToTimeOfDay (32 * 60))
        -- "due" =: (Date $ Hours $ timeToTimeOfDay (7 * 3600 + 32 * 60))

    table "size" $
        "distance" =: Array
            [ Array [Double (-455967.93489327864), Double 6.626e-34, Double 696332.8128600451]
            , Array [Double (-455967.93489327864), Double 6.626e-34, Double 696332.8128600451]
            ]
