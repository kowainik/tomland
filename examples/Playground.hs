module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Text (Text)
import Data.Time (fromGregorian)

import Toml (ParseException (..), TomlCodec, prettyToml, (.=), (<!>))
import Toml.Edsl (mkToml, table, (=:))
import Toml.Type (DateTime (..), TOML (..), Value (..))

import qualified Data.Text.IO as TIO
import qualified Toml


newtype N = N Text

data Test = Test
    { testB  :: Bool
    , testI  :: Int
    , testF  :: Double
    , testS  :: Text
    , testA  :: [Text]
    , testM  :: Maybe Bool
    , testX  :: TestInside
    , testY  :: Maybe TestInside
    , testN  :: N
    , testE1 :: Either Integer String
    , testE2 :: Either String Double
    }

newtype TestInside = TestInside { unInside :: Text }

insideT :: TomlCodec TestInside
insideT = Toml.dimap unInside TestInside $ Toml.text "inside"

testT :: TomlCodec Test
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
    <*> eitherT1 .= testE1
    <*> eitherT2 .= testE2
  where
    -- different keys for sum type
    eitherT1 :: TomlCodec (Either Integer String)
    eitherT1 = Toml.match (Toml._Integer >>> Toml._Left)  "either.Left"
           <|> Toml.match (Toml._String  >>> Toml._Right) "either.Right"

    -- same key for sum type;
    -- doesn't work if you have something like `Either String String`,
    -- you should distinguish these cases by different keys like in `eitherT1` example
    eitherT2 :: TomlCodec (Either String Double)
    eitherT2 = ( Toml.match (Toml._String >>> Toml._Left)
             <!> Toml.match (Toml._Double >>> Toml._Right)
               ) "either"

main :: IO ()
main = do
    TIO.putStrLn "=== Printing manually specified TOML ==="
    TIO.putStrLn $ prettyToml myToml

    TIO.putStrLn "=== Printing parsed TOML ==="
    content <- TIO.readFile "test.toml"
    case Toml.parse content of
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
