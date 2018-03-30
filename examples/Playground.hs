module Main where

import Data.HashMap.Strict (HashMap, fromList)
import Data.Time (fromGregorian)

import Toml.Printer (prettyToml)
import Toml.Type (AnyValue (..), DateTime (..), Key (..), TOML (..), TableId (..), Value (..))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.IO as TIO

main :: IO ()
main = TIO.putStrLn $ prettyToml myToml


myToml :: TOML
myToml = TOML (fromList
    [ (Key "a"   , AnyValue $ Bool True)
    , (Key "list", AnyValue $ Array [String "one", String "two"])
    , (Key "time", AnyValue $ Array [Date $ Day (fromGregorian 2018 3 29)])
    ] ) myInnerToml mempty

myInnerToml :: HashMap TableId TOML
myInnerToml = fromList
    [ ( TableId (NonEmpty.fromList ["table", "name", "1"])
      , TOML (fromList
            [ (Key "aInner"   , AnyValue $ Int 1)
            , (Key "listInner", AnyValue $ Array [Bool True, Bool False])
            ]) myInnerInnerToml mempty
      )
    , ( TableId (NonEmpty.fromList ["table", "name", "2"])
      , TOML (fromList [ (Key "2Inner", AnyValue $ Int 42)
                       ]) mempty mempty
      )
    ]

myInnerInnerToml :: HashMap TableId TOML
myInnerInnerToml = fromList
    [ ( TableId (NonEmpty.fromList ["table", "name", "1", "1"])
      , TOML (fromList [ (Key "aInner"   , AnyValue $ Int 1)
                       , (Key "listInner", AnyValue $ Array [Bool True, Bool False])
                       ]) mempty mempty
      )
    , ( TableId (NonEmpty.fromList ["table", "name", "1", "2"])
      , TOML (fromList [ (Key "Inner1.2", AnyValue $ Int 42)
                       ]) mempty mempty
      )
    ]
