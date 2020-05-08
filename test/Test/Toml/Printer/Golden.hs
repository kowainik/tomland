-- | This module contains golden tests for @Toml.Printer@.

module Test.Toml.Printer.Golden
       ( prettyPrinterGoldenSpec
       ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Ord (comparing)
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)
import Test.Hspec.Golden (defaultGolden)

import Toml.Type.Edsl (empty, mkToml, table, tableArray, (=:))
import Toml.Type.Key (Key (..), (<|))
import Toml.Type.Printer (PrintOptions (..), defaultOptions, prettyOptions)
import Toml.Type.TOML (TOML)
import Toml.Type.Value (Value (..))

import qualified Data.Text as T


prettyPrinterGoldenSpec :: Spec
prettyPrinterGoldenSpec = describe "Toml.Printer Golden tests" $ do
    test "pretty_default" defaultOptions
    test "pretty_sorted_only" noFormatting { printOptionsSorting = Just compare }
    test "pretty_indented_only" noFormatting { printOptionsIndent = 4 }
    test "pretty_unformatted" noFormatting
    test "pretty_custom_sorted" noFormatting { printOptionsSorting = Just spamEgg }
  where
    test :: String -> PrintOptions -> SpecWith (Arg Expectation)
    test name options = it ("Golden " ++ name) $
        defaultGolden
            ("test/golden/" ++ name ++ ".golden")
            (T.unpack $ prettyOptions options example)

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

-- | Decorate keys as tuples so spam comes before egg
spamEggDecorate :: Key -> (Int, Key)
spamEggDecorate k
    | k == "spam" = (0, "spam")
    | k == "egg" = (1, "egg")
    | otherwise = (2, k)

spamEgg :: Key -> Key -> Ordering
spamEgg = comparing spamEggDecorate
