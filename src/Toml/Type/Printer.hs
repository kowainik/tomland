{-# LANGUAGE GADTs #-}

{-# LANGUAGE PatternSynonyms #-}

{- |
Module                  : Toml.Type.Printer
Copyright               : (c) 2018-2022 Kowainik
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
       , prettyPiece
       , prettyValue
       ) where

import Data.Bifunctor (first)
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.Function (on)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (stimes)
import Data.Text (Text)
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)
import GHC.Exts (sortWith)
import Text.Printf (printf)

import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Key (Key (..), Piece (..), pattern (:||))
import Toml.Type.TOML (Entry (..), TOML (..), TableKind (..))
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
      foo = [ a
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
TOML $ HashMap.fromList
    [ ("title", EValue $ AnyValue $ Text "TOML example")
    , ("example", ETable ImplicitTable $ TOML $ HashMap.fromList
          [ ("owner", ETable HeaderTable $ TOML $ HashMap.fromList
                [ ("name", EValue $ AnyValue $ Text "Kowainik") ]
            )
          ]
      )
    ]
@

will be translated to this

@
title = "TOML Example"

[example.owner]
  name = \"Kowainik\"
@

Tables are printed according to their 'TableKind': 'HeaderTable's as
@[table]@ sections, 'DottedTable's as dotted keys, 'InlineTable's inline and
'ImplicitTable's without a header of their own.

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
prettyTomlInd options i prefix toml = concat
    [ map (tabWith options i <>) (prettyPairs options "" toml)
    , prettyTables options i prefix toml
    ]

{- | Converts a key to text, quoting the pieces that are not bare keys.

@since 0.0.0
-}
prettyKey :: Key -> Text
prettyKey = Text.intercalate "." . map prettyPiece . NonEmpty.toList . unKey
{-# INLINE prettyKey #-}

{- | Converts a key piece to text: bare if it consists of ASCII letters,
digits, @-@ and @_@ only, a quoted basic string otherwise.

@since 1.4.0.0
-}
prettyPiece :: Piece -> Text
prettyPiece (Piece p)
    | not (Text.null p) && Text.all isBareKeyChar p = p
    | otherwise = "\"" <> Text.concatMap (escapeChar False) p <> "\""
  where
    isBareKeyChar :: Char -> Bool
    isBareKeyChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '-'

{- | Lines of the form @key = value@ for a table, without indentation:
values, inline tables, and the (flattened) contents of dotted and implicit
tables.
-}
prettyPairs :: PrintOptions -> Text -> TOML -> [Text]
prettyPairs options keyPrefix = mapOrdered pairText options . entryList
  where
    pairText :: (Key, Entry) -> [Text]
    pairText (k, entry) = case entry of
        EValue (AnyValue v)  -> [key <> " = " <> prettyValue options v]
        ETable InlineTable t -> [key <> " = " <> prettyInlineTable options t]
        ETable HeaderTable _ -> []
        ETable _ t           -> prettyPairs options (key <> ".") t
        ETableArray _        -> []
      where
        key :: Text
        key = keyPrefix <> prettyKey k

{- | Returns pretty formatted tables section of the 'TOML': first all
sub-tables, then all arrays of tables.
-}
prettyTables :: PrintOptions -> Int -> Text -> TOML -> [Text]
prettyTables options i prefix toml =
    mapOrdered tableText options (entryList toml) ++ mapOrdered arrayText options (entryList toml)
  where
    tableText :: (Key, Entry) -> [Text]
    tableText (k, entry) = case entry of
        ETable HeaderTable t -> section ("[" <> addPrefix k prefix <> "]") t
        ETable InlineTable _ -> []
        ETable _ t           -> prettyTables options i (addPrefix k prefix) t
        _                    -> []

    arrayText :: (Key, Entry) -> [Text]
    arrayText (k, entry) = case entry of
        ETableArray ts -> concatMap (section ("[[" <> addPrefix k prefix <> "]]")) ts
        _              -> []

    -- Each "" results in an empty line, inserted above table names.
    -- We don't want empty lines between a table name and a subtable name.
    section :: Text -> TOML -> [Text]
    section header sub =
        "" : tabWith options i <> header :
        dropWhile (== "") (prettyTomlInd options (i + 1) name' sub)
      where
        name' :: Text
        name' = Text.dropAround (`elem` ("[]" :: String)) header

    addPrefix :: Key -> Text -> Text
    addPrefix k pref
        | Text.null pref = prettyKey k
        | otherwise      = pref <> "." <> prettyKey k

{- | Prints a 'TOML' as an inline table, e.g. @{ a = 1, b = { c = 2 } }@.
Used for tables that appear as array elements.

@since 1.4.0.0
-}
prettyInlineTable :: PrintOptions -> TOML -> Text
prettyInlineTable options toml
    | null entries = "{}"
    | otherwise    = "{ " <> Text.intercalate ", " entries <> " }"
  where
    entries :: [Text]
    entries = inlineEntries ""  toml

    inlineEntries :: Text -> TOML -> [Text]
    inlineEntries keyPrefix = mapOrdered entryText options . entryList
      where
        entryText :: (Key, Entry) -> [Text]
        entryText (k, entry) = case entry of
            EValue (AnyValue v)  -> [key <> " = " <> prettyValue options v]
            ETable DottedTable t -> inlineEntries (key <> ".") t
            ETable _ t           -> [key <> " = " <> prettyInlineTable options t]
            ETableArray ts       -> [key <> " = " <> inlineArray ts]
          where
            key :: Text
            key = keyPrefix <> prettyKey k

    inlineArray :: NonEmpty TOML -> Text
    inlineArray ts = "[" <> Text.intercalate ", " (map (prettyInlineTable options) (NonEmpty.toList ts)) <> "]"

{- | Converts a single 'Value' to text. Arrays are printed according to
'printOptionsLines'; tables are printed as inline tables.

@since 1.4.0.0
-}
prettyValue :: PrintOptions -> Value t -> Text
prettyValue options = valText
  where
    valText :: Value t -> Text
    valText (Bool b)    = Text.toLower $ showText b
    valText (Integer n) = showText n
    valText (Double d)  = showDouble d
    valText (Text s)    = showTextUnicode s
    valText (Zoned z)   = showZonedTime z
    valText (Local l)   = showText l
    valText (Day d)     = showText d
    valText (Hours h)   = showText h
    valText (Array a)   = withLines options anyText a
    valText (Table t)   = prettyInlineTable options t

    anyText :: AnyValue -> Text
    anyText (AnyValue v) = valText v

    showText :: Show a => a -> Text
    showText = Text.pack . show

    -- Basic string: escapes quotes, backslashes and control characters,
    -- and encodes all non-ASCII characters as @\U@ escapes.
    showTextUnicode :: Text -> Text
    showTextUnicode text = "\"" <> Text.concatMap (escapeChar True) text <> "\""

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

{- | Escapes one character of a basic string. Non-ASCII characters are
encoded as @\U@ escapes only when the first argument is 'True'.
-}
escapeChar :: Bool -> Char -> Text
escapeChar escapeNonAscii c = case c of
    '"'  -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\t' -> "\\t"
    '\n' -> "\\n"
    '\f' -> "\\f"
    '\r' -> "\\r"
    _ | c < ' ' || c == '\DEL'       -> Text.pack $ printf "\\u%04X" (ord c)
      | escapeNonAscii && not (isAscii c) -> Text.pack $ printf "\\U%08x" (ord c)
      | otherwise                    -> Text.singleton c

tabWith :: PrintOptions -> Int -> Text
tabWith PrintOptions{..} n = Text.replicate (n * printOptionsIndent) " "

entryList :: TOML -> [(Key, Entry)]
entryList = map (first (:|| [])) . HashMap.toList . unTOML

-- Returns a proper sorting function
mapOrdered :: ((Key, v) -> [t]) -> PrintOptions -> [(Key, v)] -> [t]
mapOrdered f options = case printOptionsSorting options of
    Just sorter -> concatMap f . sortBy (sorter `on` fst)
    Nothing     -> concatMap f . sortWith fst

{- | Print the array according to the 'printOptionsLines' option.
-}
withLines :: PrintOptions -> (AnyValue -> Text) -> [AnyValue] -> Text
withLines PrintOptions{..} valTxt a = case printOptionsLines of
    OneLine -> "[" <> Text.intercalate ", " (map valTxt a) <> "]"
    MultiLine -> "[ " <> Text.intercalate (off <> ", ") (map valTxt a) <> off <> "]"
  where
    off :: Text
    off = "\n" <> stimes printOptionsIndent " "
