module Main where

import Data.HashMap.Strict (HashMap, fromList)
import Data.Time (fromGregorian)

import Toml.Printer (toml2Text)
import Toml.Type (DateTime (..), Key (..), TOML (..), TableId (..), Value (..))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

main :: IO ()
main = putStrLn $ Text.unpack $ toml2Text myToml


myToml :: TOML
myToml = TOML (fromList
    [ (Key "a", Bool True)
    , (Key "list", Array [String "one", String "two"])
    , (Key "time", Array [Date $ Day (fromGregorian 2018 3 29)])
    ] ) myInnerToml mempty

myInnerToml :: HashMap TableId TOML
myInnerToml = fromList $ [ ( TableId (NonEmpty.fromList ["table", "name", "1"])
                           , TOML (fromList [ (Key "aInner", Int 1)
                                            , (Key "listInner", Array [Bool True, Bool False])
                                            ]) myInnerInnerToml mempty
                           )
                         , ( TableId (NonEmpty.fromList ["table", "name", "2"])
                           , TOML (fromList [ (Key "2Inner", Int 42)
                                            ]) mempty mempty
                           )
                         ]

myInnerInnerToml :: HashMap TableId TOML
myInnerInnerToml =
    fromList $ [ ( TableId (NonEmpty.fromList ["table", "name", "1", "1"])
                 , TOML (fromList [ (Key "aInner", Int 1)
                                  , (Key "listInner", Array [Bool True, Bool False])
                                  ]) mempty mempty
                 )
               , ( TableId (NonEmpty.fromList ["table", "name", "1", "2"])
                 , TOML (fromList [ (Key "Inner1.2", Int 42)
                                  ]) mempty mempty
                 )
               ]
