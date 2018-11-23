{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Text (Text)
import Data.Time (fromGregorian)
import GHC.Generics

import Toml (GenBiMap, ParseException (..), TomlCodec, pretty, (.=), (<!>))
import Toml.Edsl (mkToml, table, (=:))
import Toml.Type (DateTime (..), TOML (..), Value (..))

import qualified Data.Text.IO as TIO
import qualified Toml


newtype N = N Text

data Test = Test
    { testB    :: Bool
    , testI    :: Int
    , testF    :: Double
    , testS    :: Text
    , testA    :: [Text]
    , testM    :: Maybe Bool
    , testX    :: TestInside
    , testY    :: Maybe TestInside
    , testN    :: N
    , testE1   :: Either Integer String
    , testE2   :: Either String Double
    , testTree :: MyTree Text
    }

newtype TestInside = TestInside { unInside :: Text }

data MyTree a
  = MyLeaf a
  | MyNode Int a (MyTree a) (MyTree a)
  deriving (Show, Generic)

instance (GenBiMap a) => GenBiMap (MyTree a)

insideT :: TomlCodec TestInside
insideT = Toml.dimap unInside TestInside $ Toml.text "inside"

testT :: TomlCodec Test
testT = Test
    <$> Toml.bool "testB" .= testB
    <*> Toml.int "testI" .= testI
    <*> Toml.double "testF" .= testF
    <*> Toml.text "testS" .= testS
    <*> Toml.arrayOf Toml._Text "testA" .= testA
    <*> Toml.dioptional (Toml.bool "testM") .= testM
    <*> Toml.table insideT "testX" .= testX
    <*> Toml.dioptional (Toml.table insideT "testY") .= testY
    <*> Toml.diwrap (Toml.text "testN") .= testN
    <*> eitherT1 .= testE1
    <*> eitherT2 .= testE2
    <*> Toml.generic "testTree" .= testTree
  where
    -- different keys for sum type
    eitherT1 :: TomlCodec (Either Integer String)
    eitherT1 = Toml.match (Toml._Left >>> Toml._Integer)  "either.Left"
           <|> Toml.match (Toml._Right >>> Toml._String) "either.Right"

    -- same key for sum type;
    -- doesn't work if you have something like `Either String String`,
    -- you should distinguish these cases by different keys like in `eitherT1` example
    eitherT2 :: TomlCodec (Either String Double)
    eitherT2 = ( Toml.match (Toml._Left >>> Toml._String)
             <!> Toml.match (Toml._Right >>> Toml._Double)
               ) "either"

main :: IO ()
main = do
    TIO.putStrLn "=== Printing manually specified TOML ==="
    TIO.putStrLn $ pretty myToml

    TIO.putStrLn "=== Printing parsed TOML ==="
    content <- TIO.readFile "examples/test.toml"
    case Toml.parse content of
        Left (ParseException e) -> TIO.putStrLn e
        Right toml              -> TIO.putStrLn $ pretty toml

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
