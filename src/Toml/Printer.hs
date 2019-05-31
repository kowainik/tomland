{-# LANGUAGE TypeFamilies #-}

{- | Contains functions for pretty printing @toml@ types. -}

module Toml.Printer
       ( PrintOptions(..)
       , defaultOptions
       , pretty
       , prettyOptions
       , prettyKey
       ) where

import Data.HashMap.Strict (HashMap)
import Data.List (sortBy, splitAt)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)

import Toml.PrefixTree (Key (..), Piece (..), PrefixMap, PrefixTree (..))
import Toml.Type (AnyValue (..), TOML (..), Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

{- | Configures the pretty printer. -}
data PrintOptions = PrintOptions
    { -- | How table keys should be sorted, if at all.
      printOptionsSorting :: !(Maybe (Key -> Key -> Ordering))
      -- | Number of spaces by which to indent.
    , printOptionsIndent  :: !Int
    }

{- | Default printing options.

1. Sorts all keys and tables by name.
2. Indents with 2 spaces.
-}
defaultOptions :: PrintOptions
defaultOptions = PrintOptions (Just compare) 2

{- | Converts 'TOML' type into 'Data.Text.Text' (using 'defaultOptions').

For example, this

@
TOML
    { tomlPairs  = HashMap.fromList
          [("title", AnyValue $ Text "TOML example")]
    , tomlTables = PrefixTree.fromList
          [( "example" <| "owner"
           , mempty
                 { tomlPairs  = HashMap.fromList
                       [("name", AnyValue $ Text "Kowainik")]
                 }
           )]
    , tomlTableArrays = mempty
    }
@

will be translated to this

@
title = "TOML Example"

[example.owner]
  name = \"Kowainik\"
@
-}
pretty :: TOML -> Text
pretty = prettyOptions defaultOptions

-- | Converts 'TOML' type into 'Data.Text.Text' using provided 'PrintOptions'
prettyOptions :: PrintOptions -> TOML -> Text
prettyOptions options = Text.unlines . prettyTomlInd options 0 ""

-- | Converts 'TOML' into a list of 'Data.Text.Text' elements with the given indent.
prettyTomlInd :: PrintOptions -- ^ Printing options
              -> Int          -- ^ Current indentation
              -> Text         -- ^ Accumulator for table names
              -> TOML         -- ^ Given 'TOML'
              -> [Text]       -- ^ Pretty result
prettyTomlInd options i prefix TOML{..} = concat
    [ prettyKeyValue    options i tomlPairs
    , prettyTables      options i prefix tomlTables
    , prettyTableArrays options i prefix tomlTableArrays
    ]

-- | Converts a key to text
prettyKey :: Key -> Text
prettyKey = Text.intercalate "." . map unPiece . NonEmpty.toList . unKey

-- | Returns pretty formatted  key-value pairs of the 'TOML'.
prettyKeyValue :: PrintOptions -> Int -> HashMap Key AnyValue -> [Text]
prettyKeyValue options i = mapOrdered (\kv -> [kvText kv]) options
  where
    kvText :: (Key, AnyValue) -> Text
    kvText (k, AnyValue v) =
      tabWith options i <> prettyKey k <>  " = " <> valText v

    valText :: Value t -> Text
    valText (Bool b)    = Text.toLower $ showText b
    valText (Integer n) = showText n
    valText (Double d)  = showDouble d
    valText (Text s)    = showText s
    valText (Zoned z)   = showZonedTime z
    valText (Local l)   = showText l
    valText (Day d)     = showText d
    valText (Hours h)   = showText h
    valText (Array a)   = "[" <> Text.intercalate ", " (map valText a) <> "]"

    showText :: Show a => a -> Text
    showText = Text.pack . show

    showDouble :: Double -> Text
    showDouble d | isInfinite d && d < 0 = "-inf"
                 | isInfinite d = "inf"
                 | isNaN d = "nan"
                 | otherwise = showText d

    showZonedTime :: ZonedTime -> Text
    showZonedTime t = Text.pack $ showZonedDateTime t <> showZonedZone t
      where
        showZonedDateTime = formatTime defaultTimeLocale "%FT%T%Q"
        showZonedZone
            = (\(x,y) -> x ++ ":" ++ y)
            . (\z -> splitAt (length z - 2) z)
            . formatTime defaultTimeLocale "%z"

-- | Returns pretty formatted tables section of the 'TOML'.
prettyTables :: PrintOptions -> Int -> Text -> PrefixMap TOML -> [Text]
prettyTables options i pref = mapOrdered (prettyTable . snd) options
  where
    prettyTable :: PrefixTree TOML -> [Text]
    prettyTable (Leaf k toml) =
        let name = addPrefix k pref
        -- Each "" results in an empty line, inserted above table names
        in "": tabWith options i <> prettyTableName name :
        -- We don't want empty lines between a table name and a subtable name
             dropWhile (== "") (prettyTomlInd options (i + 1) name toml)

    prettyTable (Branch k mToml prefMap) =
        let name  = addPrefix k pref
            nextI = i + 1
            toml  = case mToml of
                        Nothing -> []
                        Just t  -> prettyTomlInd options nextI name t
        -- Each "" results in an empty line, inserted above table names
        in "": tabWith options i <> prettyTableName name :
        -- We don't want empty lines between a table name and a subtable name
             dropWhile (== "") (toml ++ prettyTables options nextI name prefMap)

    prettyTableName :: Text -> Text
    prettyTableName n = "[" <> n <> "]"

prettyTableArrays :: PrintOptions -> Int -> Text -> HashMap Key (NonEmpty TOML) -> [Text]
prettyTableArrays options i pref = mapOrdered arrText options
  where
    arrText :: (Key, NonEmpty TOML) -> [Text]
    arrText (k, ne) =
      let name = addPrefix k pref
          render toml =
            -- Each "" results in an empty line, inserted above array names
            "": tabWith options i <> "[[" <> name <> "]]" :
            -- We don't want empty lines between an array name and a subtable name
              dropWhile (== "") (prettyTomlInd options (i + 1) name toml)
      in concatMap render $ NonEmpty.toList ne

-----------------------------------------------------
-- Helper functions
-----------------------------------------------------

-- Returns an indentation prefix
tabWith :: PrintOptions -> Int -> Text
tabWith PrintOptions{..} n = Text.replicate (n * printOptionsIndent) " "

-- Returns a proper sorting function
mapOrdered :: ((Key, v) -> [t]) -> PrintOptions -> HashMap Key v -> [t]
mapOrdered f options = case printOptionsSorting options of
    Just sorter -> concatMap f . sortBy (applyToFirst sorter) . HashMap.toList
    Nothing     -> concatMap f . HashMap.toList

-- Applies a binary function to the first elements of tuples
applyToFirst :: (a -> b -> c) -> (a, x) -> (b, y) -> c
applyToFirst f x y = f (fst x) (fst y)

-- Adds next part of the table name to the accumulator.
addPrefix :: Key -> Text -> Text
addPrefix key = \case
    "" -> prettyKey key
    prefix -> prefix <> "." <> prettyKey key
