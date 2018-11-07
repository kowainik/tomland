{-# LANGUAGE TypeFamilies #-}

{- | Contains functions for pretty printing @toml@ types. -}

module Toml.Printer
       ( PrintOptions(..)
       , defaultOptions
       , pretty
       , prettyOptions
       , prettyTomlInd
       ) where

import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
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

-- Returns an indentation prefix
tabWith :: PrintOptions -> Int -> Text
tabWith options n =
    Text.cons '\n' (Text.replicate (n * indent options) " ")

-- Returns a proper sorting function
orderWith :: Ord k => PrintOptions -> [(k, v)] -> [(k, v)]
orderWith options
    | shouldSort options = sortOn fst
    | otherwise          = id

{- | Converts 'TOML' type into 'Text' (using 'defaultOptions').

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
prettyOptions options = flip Text.snoc '\n' . Text.drop 1 . prettyTomlInd options 0 ""

-- | Converts 'TOML' into 'Text' with the given indent.
prettyTomlInd :: PrintOptions -- ^ Printing options
              -> Int          -- ^ Current indentation
              -> Text         -- ^ Accumulator for table names
              -> TOML         -- ^ Given 'TOML'
              -> Text         -- ^ Pretty result
prettyTomlInd options i prefix TOML{..} =
  Text.intercalate "\n" $ filter (not . Text.null)
    [ prettyKeyValue options i tomlPairs
    , prettyTables options i prefix tomlTables
    , prettyTableArrays options i prefix tomlTableArrays
    ]


-- | Returns pretty formatted  key-value pairs of the 'TOML'.
prettyKeyValue :: PrintOptions -> Int -> HashMap Key AnyValue -> Text
prettyKeyValue options i =
    Text.concat . map kvText . orderWith options . HashMap.toList
  where
    kvText :: (Key, AnyValue) -> Text
    kvText (k, AnyValue v) = mconcat
        [ tabWith options i
        , prettyKey k
        , " = "
        , valText v ]

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
prettyTables options i pref =
    Text.intercalate "\n" . map (prettyTable . snd) . orderWith options . HashMap.toList
  where
    prettyTable :: PrefixTree TOML -> Text
    prettyTable (Leaf k toml) =
        let name = getPref k in mconcat
            [ tabWith options i
            , prettyTableName name
            , prettyTomlInd options (succ i) name toml ]
    prettyTable (Branch k mToml prefMap) =
        let name  = getPref k
            nextI = succ i
            toml  = case mToml of
                        Nothing -> ""
                        Just t  -> prettyTomlInd options nextI name t
        in mconcat
            [ tabWith options i
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

prettyTableArrays :: PrintOptions -> Int -> Text -> HashMap Key (NonEmpty TOML) -> Text
prettyTableArrays options i pref =
    Text.intercalate "\n" . map arrText . orderWith options . HashMap.toList
  where
    arrText :: (Key, NonEmpty TOML) -> Text
    arrText (k, ne) =
      let name = getPref k
          render toml = mconcat
            [ tabWith options i
            , prettyTableArrayName name
            , prettyTomlInd options (succ i) name toml
            ]
      in Text.intercalate "\n" $ map render $ NonEmpty.toList ne

    -- Adds next part of the table name to the accumulator.
    getPref :: Key -> Text
    getPref k = case pref of
        "" -> prettyKey k
        _  -> pref <> "." <> prettyKey k

    prettyTableArrayName :: Text -> Text
    prettyTableArrayName n = "[[" <> n <> "]]"
