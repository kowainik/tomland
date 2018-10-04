-- | This module contains golden tests for @Toml.Printer@.

module Test.Toml.Printer.Golden
       ( test_prettyGolden
       ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Silver (goldenVsAction)
import Toml (AnyValue (..), TOML (..), Value (..))
import Toml.PrefixTree ((<|))
import Toml.Printer (PrintOptions (..), pretty, prettyOptions)

import qualified Data.HashMap.Strict as HashMap
import qualified Toml.PrefixTree as PrefixTree

example :: TOML
example = mempty
    { tomlPairs  = HashMap.fromList
          [ ("b", AnyValue $ Text "bb")
          , ("a", AnyValue $ Text "a")
          , ("d", AnyValue $ Text "ddd")
          , ("c", AnyValue $ Text "cccc") ]
    , tomlTables = PrefixTree.fromList
          [ ( "qux" <| "doo"
            , mempty
                  { tomlPairs = HashMap.fromList
                        [ ("spam", AnyValue $ Text "!")
                        , ("egg", AnyValue $ Text "?") ]
                  }
            )
          , ("foo", mempty)
          , ("doo", mempty)
          , ("baz", mempty)
          ]
    }

test_prettyGolden :: TestTree
test_prettyGolden =
    testGroup "Toml.Printer"
        [ test pretty "test/golden/pretty_default.golden"
        , test (prettyOptions $ PrintOptions True 0)
              "test/golden/pretty_sorted_only.golden"
        , test (prettyOptions $ PrintOptions False 4)
              "test/golden/pretty_indented_only.golden"
        , test (prettyOptions $ PrintOptions False 0)
              "test/golden/pretty_unformatted.golden"
        ]
  where
    test prn g = goldenVsAction "example" g (pure $ prn example) id
