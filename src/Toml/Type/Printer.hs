{-# LANGUAGE TypeFamilies #-}

{- |
Module                  : Toml.Type.Printer
Copyright               : (c) 2018-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Contains functions for pretty printing @toml@ types.

@since 0.0.0
-}

module Toml.Type.Printer
       ( PrintOptions(..)
       , Lines(..)
       , defaultOptions
       , pretty
       , prettyOptions
       , prettyKey
       ) where

import GHC.Exts (sortWith)
import Data.Bifunctor (first)
import Data.Char (isAscii, ord)
import Data.Coerce (coerce)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.List (sortBy, foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (stimes)
import Data.Text (Text)
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)

import Text.Printf (printf)

import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Key (Key (..), Piece (..))
import Toml.Type.PrefixTree (PrefixMap, PrefixTree (..))
import Toml.Type.TOML (TOML (..))
import Toml.Type.Value (Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text



{- | Configures the pretty printer.

@since 0.5.0
-}
data PrintOptions = PrintOptions
    { {- | How table keys should be sorted, if at all.

      @since 1.1.0.0
      -}
      printOptionsSorting :: !(Maybe (Key -> Key -> Ordering))

      {- | Number of spaces by which to indent.

      @since 1.1.0.0
      -}
    , printOptionsIndent  :: !Int
    {- | How to print Array.
      OneLine:

      @
      foo = [a, b]
      @

      MultiLine:

      @
      foo =
          [ a
          , b
          ]
      @

      Default is 'OneLine'.
    -}
    , printOptionsLines :: !Lines
    }

{- | Default printing options.

1. Sorts all keys and tables by name.
2. Indents with 2 spaces.

@since 0.5.0
-}
defaultOptions :: PrintOptions
defaultOptions = PrintOptions (Just compare) 2 OneLine

data Lines = OneLine | MultiLine

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

@since 0.0.0
-}
pretty :: TOML -> Text
pretty = prettyOptions defaultOptions

{- | Converts 'TOML' type into 'Data.Text.Text' using provided 'PrintOptions'

@since 0.5.0
-}
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

{- | Converts a key to text

@since 0.0.0
-}
prettyKey :: Key -> Text
prettyKey = Text.intercalate "." . NonEmpty.toList . coerce
{-# INLINE prettyKey #-}

-- | Returns pretty formatted  key-value pairs of the 'TOML'.
prettyKeyValue :: PrintOptions -> Int -> HashMap Key AnyValue -> [Text]
prettyKeyValue options i = mapOrdered (\kv -> [kvText kv]) options . HashMap.toList
  where
    kvText :: (Key, AnyValue) -> Text
    kvText (k, AnyValue v) =
      tabWith options i <> prettyKey k <> " = " <> valText v

    valText :: Value t -> Text
    valText (Bool b)    = Text.toLower $ showText b
    valText (Integer n) = showText n
    valText (Double d)  = showDouble d
    valText (Text s)    = showTextUnicode s
    valText (Zoned z)   = showZonedTime z
    valText (Local l)   = showText l
    valText (Day d)     = showText d
    valText (Hours h)   = showText h
    valText (Array a)   = withLines options valText a

    showText :: Show a => a -> Text
    showText = Text.pack . show


    -- | Function encodes all non-ascii characters in TOML defined form using the isAscii function
    showTextUnicode :: Text -> Text
    showTextUnicode text = Text.pack $ show finalText
      where
        xss = Text.unpack text
        finalText = foldl' (\acc (ch, asciiCh) -> acc ++ getCh ch asciiCh) "" asciiArr

        asciiArr = zip xss $ asciiStatus xss

        getCh :: Char -> Bool -> String
        getCh ch True  = [ch] -- it is true ascii character
        getCh ch False = printf "\\U%08x" (ord ch) :: String -- it is not true ascii character, it must be encoded

        asciiStatus :: String -> [Bool]
        asciiStatus = map isAscii

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
prettyTables options i pref asPieces = mapOrdered (prettyTable . snd) options asKeys
  where
    asKeys :: [(Key, PrefixTree TOML)]
    asKeys = map (first pieceToKey) $ HashMap.toList asPieces

    pieceToKey :: Piece -> Key
    pieceToKey = Key . pure

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
prettyTableArrays options i pref = mapOrdered arrText options . HashMap.toList
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
mapOrdered :: ((Key, v) -> [t]) -> PrintOptions -> [(Key, v)] -> [t]
mapOrdered f options = case printOptionsSorting options of
    Just sorter -> concatMap f . sortBy (sorter `on` fst)
    Nothing     -> concatMap f . sortWith fst

-- Adds next part of the table name to the accumulator.
addPrefix :: Key -> Text -> Text
addPrefix key = \case
    "" -> prettyKey key
    prefix -> prefix <> "." <> prettyKey key

withLines :: PrintOptions -> (Value t -> Text) -> [Value t] -> Text
withLines PrintOptions{..} valTxt a = case printOptionsLines of
    OneLine -> "[" <> Text.intercalate ", " (map valTxt a) <> "]"
    MultiLine -> off <> "[ " <> Text.intercalate (off <> ", ") (map valTxt a) <> off <> "]"
  where
    off :: Text
    off = "\n" <> stimes printOptionsIndent " "
