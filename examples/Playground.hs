module Main where

import Data.Text (Text)
import Data.Time (fromGregorian)

import Toml.Bi (BiToml, (.=))
import Toml.Edsl (table, toml, (=:))
import Toml.Parser (ParseException (..), parse)
import Toml.Printer (prettyToml)
import Toml.Type (DateTime (..), TOML (..), Value (..))

import qualified Data.Text.IO as TIO
import qualified Toml

data Test = Test
    { testB :: Bool
    , testI :: Int
    , testF :: Double
    , testS :: Text
    , testA :: [Text]
    , testM :: Maybe Bool
    , testX :: TestInside
    , testY :: Maybe TestInside
    }

newtype TestInside = TestInside { unInside :: Text }

insideT :: BiToml TestInside
insideT = Toml.dimap unInside TestInside $ Toml.str "inside"

testT :: BiToml Test
testT = Test
    <$> Toml.bool   "testB" .= testB
    <*> Toml.int    "testI" .= testI
    <*> Toml.double "testF" .= testF
    <*> Toml.str    "testS" .= testS
    <*> Toml.arrayOf Toml.strV "testA" .= testA
    <*> Toml.maybeP Toml.bool "testM" .= testM
    <*> Toml.table insideT "testX" .= testX
    <*> Toml.maybeP (Toml.table insideT) "testY" .= testY

main :: IO ()
main = do
    TIO.putStrLn "=== Printing manually specified TOML ==="
    TIO.putStrLn $ prettyToml myToml

    TIO.putStrLn "=== Printing parsed TOML ==="
    content <- TIO.readFile "test.toml"
    case parse content of
        Left (ParseException e) -> TIO.putStrLn e
        Right tml               -> TIO.putStrLn $ prettyToml tml

    TIO.putStrLn "=== Testing bidirectional conversion ==="
    biFile <- TIO.readFile "examples/biTest.toml"
    TIO.putStrLn $ case Toml.decode testT biFile of
        Left msg   -> Toml.prettyException msg
        Right test -> Toml.encode testT test

myToml :: TOML
myToml = toml $ do
    "a" =: Bool True
    "list" =: Array [String "one", String "two"]
    "time" =: Array [Date $ Day (fromGregorian 2018 3 29)]
    table "table.name.1" $ do
        "aInner" =: Int 1
        "listInner" =: Array [Bool True, Bool False]
        table "1" $ do
            "aInner11" =: Int 11
            "listInner11" =: Array [Int 0, Int 1]
        table "2" $
            "Inner12" =: Int 12
    table "table.name.2" $
        "Inner2" =: Int 42
