module Main where

import Data.Text (Text)
import Data.Time (fromGregorian)

import Toml.Bi (BiToml, (.=))
import Toml.Parser (ParseException (..), parse)
import Toml.PrefixTree (PrefixMap, fromList)
import Toml.Printer (prettyToml)
import Toml.Type (AnyValue (..), DateTime (..), TOML (..), Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.IO as TIO
import qualified Toml

data Test = Test
  { testB :: Bool
  , testI :: Int
  , testF :: Double
  , testS :: Text
  , testA :: [Text]
  }

testT :: BiToml Test
testT = Test
 <$> Toml.bool   "testB" .= testB
 <*> Toml.int    "testI" .= testI
 <*> Toml.double "testF" .= testF
 <*> Toml.str    "testS" .= testS
 <*> Toml.arrayOf Toml.strV "testA" .= testA

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
    case Toml.encode testT biFile of
        Left msg   -> print msg
        Right test -> TIO.putStrLn $ Toml.unsafeDecode testT test

myToml :: TOML
myToml = TOML (HashMap.fromList
    [ ("a"   , AnyValue $ Bool True)
    , ("list", AnyValue $ Array [String "one", String "two"])
    , ("time", AnyValue $ Array [Date $ Day (fromGregorian 2018 3 29)])
    ] ) myInnerToml

myInnerToml :: PrefixMap TOML
myInnerToml = fromList
    [ ( "table.name.1"
      , TOML (HashMap.fromList
            [ ("aInner"   , AnyValue $ Int 1)
            , ("listInner", AnyValue $ Array [Bool True, Bool False])
            ]) myInnerInnerToml
      )
    , ( "table.name.2"
      , TOML (HashMap.fromList [("2Inner", AnyValue $ Int 42)]) mempty
      )
    ]


myInnerInnerToml :: PrefixMap TOML
myInnerInnerToml = fromList
    [ ( "table.name.1.1"
      , TOML (HashMap.fromList
            [ ("aInner"   , AnyValue $ Int 1)
            , ("listInner", AnyValue $ Array [Bool True, Bool False])
            ]) mempty
      )
    , ( "table.name.1.2"
      , TOML (HashMap.fromList [("Inner1.2", AnyValue $ Int 42)]) mempty
      )
    ]
