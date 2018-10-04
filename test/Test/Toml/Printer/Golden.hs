-- | This module contains golden tests for @Toml.Printer@.

module Test.Toml.Printer.Golden
       ( test_prettyGolden
       ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Silver (goldenVsAction)
import Toml (TOML)
import Toml.Edsl ((=:), mkToml, table)
import Toml.PrefixTree ((<|))
import Toml.Printer (PrintOptions (..), defaultOptions, prettyOptions)

example :: TOML
example = mkToml $ do
    "b" =: "bb"
    "a" =: "a"
    "d" =: "ddd"
    "c" =: "cccc"
    table ("qux" <| "doo") $ do
      "spam" =: "!"
      "egg" =: "?"
    table "foo" $ pure ()
    table "doo" $ pure ()
    table "baz" $ pure ()

noFormatting :: PrintOptions
noFormatting = PrintOptions
    { shouldSort = False
    , indent     = 0
    }

test_prettyGolden :: TestTree
test_prettyGolden =
    testGroup "Toml.Printer"
        [ test "pretty_default" defaultOptions
        , test "pretty_sorted_only" noFormatting{ shouldSort = True} 
        , test "pretty_indented_only" noFormatting{ indent = 4 }
        , test "pretty_unformatted" noFormatting
        ]
  where
    test name options =
        goldenVsAction name ("test/golden/" ++ name ++ ".golden")
            (pure $ prettyOptions options example) id
