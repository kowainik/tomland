{-# LANGUAGE TypeFamilies #-}

{- | Contains functions for pretty printing @toml@ types. -}

module Toml.Printer
       ( PrintOptions(..)
       , pretty
       , prettyOptions
       , prettyTomlInd
       ) where

import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>), mconcat)
import Data.Text (Text)
import Data.Time (formatTime, defaultTimeLocale, ZonedTime)
import Data.List (splitAt, sortOn)

import Toml.PrefixTree (Key (..), Piece (..), PrefixMap, PrefixTree (..))
import Toml.Type (AnyValue (..), DateTime (..), TOML (..), Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

{- | Configures the pretty printer. -}
data PrintOptions = PrintOptions
    { shouldSort :: Bool  -- ^ should table keys be sorted ot shouldn't
    , indent     :: Int   -- ^ indentation size
    } deriving (Show)

{- | Default printing options. -}
defaultOptions :: PrintOptions
defaultOptions = PrintOptions True 2

-- Tab is equal to 2 spaces now.
tab :: Int -> Text
tab n = Text.cons '\n' (Text.replicate (2*n) " ")

{- | Converts 'TOML' type into 'Text' (using 'defaultOptions').

For example, this

@
TOML
    { tomlPairs  = HashMap.fromList [(Key "title", String "TOML example")]
    , tomlTables = HashMap.fromList
          [( TableId (NonEmpty.fromList ["example", "owner"])
           , TOML
                 { tomlPairs  = HashMap.fromList [(Key "name", String "Kowainik")]
                 , tomlTables = mempty
                 , tomlTableArrays = mempty
                 }
           )]
    , tomlTableArrays = mempty
    }
@

will be translated to this

@
title = "TOML Example"

[example.owner]
  name = "Kowainik"

@
-}
pretty :: TOML -> Text
pretty = prettyOptions defaultOptions

{- | Converts 'TOML' type into 'Text' using provided 'PrintOptions' -}
prettyOptions :: PrintOptions -> TOML -> Text
prettyOptions options = Text.drop 1 . prettyTomlInd options 0 ""

-- | Converts 'TOML' into 'Text' with the given indent.
prettyTomlInd :: PrintOptions -- ^ Printing options
              -> Int          -- ^ Current indentation
              -> Text         -- ^ Accumulator for table names
              -> TOML         -- ^ Given 'TOML'
              -> Text         -- ^ Pretty result
prettyTomlInd options i prefix TOML{..} =
    prettyKeyValue options i tomlPairs <> "\n"
    <> prettyTables options i prefix tomlTables

-- | Returns pretty formatted  key-value pairs of the 'TOML'.
prettyKeyValue :: PrintOptions -> Int -> HashMap Key AnyValue -> Text
prettyKeyValue options i = Text.concat . map kvText . order . HashMap.toList
  where
    order | shouldSort options = sortOn fst
          | otherwise          = id
    kvText :: (Key, AnyValue) -> Text
    kvText (k, AnyValue v) = tab i <> prettyKey k <> " = " <> valText v

    valText :: Value t -> Text
    valText (Bool b)    = Text.toLower $ showText b
    valText (Integer n) = showText n
    valText (Double d)  = showDouble d
    valText (Text s)    = showText s
    valText (Date d)    = timeText d
    valText (Array a)   = "[" <> Text.intercalate ", " (map valText a) <> "]"

    timeText :: DateTime -> Text
    timeText (Zoned z) = showZonedTime z
    timeText (Local l) = showText l
    timeText (Day d)   = showText d
    timeText (Hours h) = showText h

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
prettyTables :: PrintOptions -> Int -> Text -> PrefixMap TOML -> Text
prettyTables options i pref = Text.concat . map prettyTable . HashMap.elems
  where
    prettyTable :: PrefixTree TOML -> Text
    prettyTable (Leaf k toml) =
        let name = getPref k in
        tab i <> prettyTableName name
              <> prettyTomlInd options (i + indent options) name toml
    prettyTable (Branch k mToml prefMap) =
        let name  = getPref k
            nextI = i + indent options
            toml  = case mToml of
                        Nothing -> ""
                        Just t  -> prettyTomlInd options nextI name t
        in mconcat
            [ tab i
            , prettyTableName name
            , toml
            , prettyTables options nextI name prefMap ]

    -- Adds next part of the table name to the accumulator.
    getPref :: Key -> Text
    getPref k = case pref of
        "" -> prettyKey k
        _  -> pref <> "." <> prettyKey k

    prettyTableName :: Text -> Text
    prettyTableName n = "[" <> n <> "]"

prettyKey :: Key -> Text
prettyKey (Key k) = Text.intercalate "." $ map unPiece (NonEmpty.toList k)
