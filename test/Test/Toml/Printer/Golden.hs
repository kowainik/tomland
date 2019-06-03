-- | This module contains golden tests for @Toml.Printer@.

module Test.Toml.Printer.Golden
       ( test_prettyGolden
       ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Silver (goldenVsAction)

import Toml (TOML, Value (..))
import Toml.Edsl (empty, mkToml, table, tableArray, (=:))
import Toml.PrefixTree (Key (..), Piece (..), (<|))
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
    table "foo" empty
    table "doo" empty
    table "baz" empty
    tableArray "deepest" $
      "ping" =: "pong"
      :| [empty]
    tableArray "deeper" $
      "green" =: Bool True
      :| [table "blue" ("red" =: Integer 255)]

noFormatting :: PrintOptions
noFormatting = PrintOptions
    { printOptionsSorting = Nothing
    , printOptionsIndent  = 0
    }

-- | Sorts "spam" to the beginning and "egg" to the end
spamEgg :: Key -> Key -> Ordering
spamEgg k1 k2
    | (k1 == fromText "spam") || (k2 == fromText "egg") = LT
    | (k1 == fromText "egg") || (k2 == fromText "spam") = GT
    | otherwise                                         = compare k1 k2
  where
    fromText :: Text -> Key
    fromText x = Key $ pure $ Piece x

test_prettyGolden :: TestTree
test_prettyGolden =
    testGroup "Toml.Printer"
        [ test "pretty_default" defaultOptions
        , test "pretty_sorted_only" noFormatting { printOptionsSorting = Just compare }
        , test "pretty_indented_only" noFormatting { printOptionsIndent = 4 }
        , test "pretty_unformatted" noFormatting
        , test "pretty_custom_sorted" noFormatting { printOptionsSorting = Just spamEgg }
        ]
  where
    test name options =
        goldenVsAction name ("test/golden/" ++ name ++ ".golden")
            (pure $ prettyOptions options example) id
