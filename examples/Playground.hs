module Main where

import Data.HashMap.Strict (HashMap, fromList)
import Data.Text (Text)
import Data.Time (fromGregorian)

import Toml.Parser (ParseException (..), parse)
import Toml.Printer (prettyToml)
import Toml.Type (AnyValue (..), DateTime (..), Key (..), TOML (..), Value (..))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "=== Printing manually specified TOML ==="
    TIO.putStrLn $ prettyToml myToml

    TIO.putStrLn "=== Printing parsed TOML ==="
    content <- TIO.readFile "test.toml"
    case parse content of
        Left (ParseException e) -> TIO.putStrLn e
        Right toml              -> TIO.putStrLn $ prettyToml toml

key :: [Text] -> Key
key = Key . NonEmpty.fromList

myToml :: TOML
myToml = TOML (fromList
    [ (key ["a"]   , AnyValue $ Bool True)
    , (key ["list"], AnyValue $ Array [String "one", String "two"])
    , (key ["time"], AnyValue $ Array [Date $ Day (fromGregorian 2018 3 29)])
    ] ) myInnerToml

myInnerToml :: HashMap Key TOML
myInnerToml = fromList
    [ ( key ["table", "name", "1"]
      , TOML (fromList
            [ (key ["aInner"]   , AnyValue $ Int 1)
            , (key ["listInner"], AnyValue $ Array [Bool True, Bool False])
            ]) myInnerInnerToml
      )
    , ( key ["table", "name", "2"]
      , TOML (fromList [ (key ["2Inner"], AnyValue $ Int 42)
                       ]) mempty
      )
    ]

myInnerInnerToml :: HashMap Key TOML
myInnerInnerToml = fromList
    [ ( key ["table", "name", "1", "1"]
      , TOML (fromList [ (key ["aInner"]   , AnyValue $ Int 1)
                       , (key ["listInner"], AnyValue $ Array [Bool True, Bool False])
                       ]) mempty
      )
    , ( key ["table", "name", "1", "2"]
      , TOML (fromList [ (key ["Inner1.2"], AnyValue $ Int 42)
                       ]) mempty
      )
    ]
