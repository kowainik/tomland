module Main where

import Data.Text (Text)
import Data.Time (fromGregorian)

import Toml.Bi (BiToml, (.=))
import Toml.Edsl (mkToml, table, (=:))
import Toml.Parser (ParseException (..), parse)
import Toml.Printer (prettyToml)
import Toml.Type (DateTime (..), TOML (..), Value (..))

import qualified Data.Text.IO as TIO
import qualified Toml

newtype N = N Text

data Test = Test
    { testB :: Bool
    , testI :: Int
    , testF :: Double
    , testS :: Text
    , testA :: [Text]
    , testM :: Maybe Bool
    , testX :: TestInside
    , testY :: Maybe TestInside
    , testN :: N
    }

newtype TestInside = TestInside { unInside :: Text }

insideT :: BiToml TestInside
insideT = Toml.dimap unInside TestInside $ Toml.text "inside"

testT :: BiToml Test
testT = Test
    <$> Toml.bool "testB" .= testB
    <*> Toml.int "testI" .= testI
    <*> Toml.double "testF" .= testF
    <*> Toml.text "testS" .= testS
    <*> Toml.arrayOf Toml._Text "testA" .= testA
    <*> Toml.maybeT Toml.bool "testM" .= testM
    <*> Toml.table insideT "testX" .= testX
    <*> Toml.maybeT (Toml.table insideT) "testY" .= testY
    <*> Toml.wrapper Toml.text "testN" .= testN

main :: IO ()
main = do
    TIO.putStrLn "=== Printing manually specified TOML ==="
    TIO.putStrLn $ prettyToml myToml

    TIO.putStrLn "=== Printing parsed TOML ==="
    content <- TIO.readFile "test.toml"
    case parse content of
        Left (ParseException e) -> TIO.putStrLn e
        Right toml              -> TIO.putStrLn $ prettyToml toml

    TIO.putStrLn "=== Testing bidirectional conversion ==="
    biFile <- TIO.readFile "examples/biTest.toml"
    TIO.putStrLn $ case Toml.decode testT biFile of
        Left msg   -> Toml.prettyException msg
        Right test -> Toml.encode testT test

myToml :: TOML
myToml = mkToml $ do
    "a" =: Bool True
    "list" =: Array ["one", "two"]
    "time" =: Array [Date $ Day (fromGregorian 2018 3 29)]
    table "table.name.1" $ do
        "aInner" =: 1
        "listInner" =: Array [Bool True, Bool False]
        table "1" $ do
            "aInner11" =: 11
            "listInner11" =: Array [0, 1]
        table "2" $
            "Inner12" =: "12"
    table "table.name.2" $
        "Inner2" =: 42
