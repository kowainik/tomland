{-# LANGUAGE TypeFamilies #-}

{- | Contains functions for pretty printing @toml@ types. -}

module Toml.Printer
       ( prettyToml
       , prettyTomlInd
       ) where

import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (formatTime, defaultTimeLocale, ZonedTime)
import Data.List (splitAt)

import Toml.PrefixTree (Key (..), Piece (..), PrefixMap, PrefixTree (..))
import Toml.Type (AnyValue (..), DateTime (..), TOML (..), Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

-- Tab is equal to 2 spaces now.
tab :: Int -> Text
tab n = Text.cons '\n' (Text.replicate (2*n) " ")

{- | Converts 'TOML' type into 'Text'.

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
prettyToml :: TOML -> Text
prettyToml = Text.drop 1 . prettyTomlInd 0 ""

-- | Converts 'TOML' into 'Text' with the given indent.
prettyTomlInd :: Int  -- ^ Number of spaces for indentation
              -> Text -- ^ Accumulator for table names
              -> TOML -- ^ Given 'TOML'
              -> Text -- ^ Pretty result
prettyTomlInd i prefix TOML{..} = prettyKeyValue i tomlPairs <> "\n"
                               <> prettyTables i prefix tomlTables

-- | Returns pretty formatted  key-value pairs of the 'TOML'.
prettyKeyValue :: Int -> HashMap Key AnyValue -> Text
prettyKeyValue i = Text.concat . map kvText . HashMap.toList
  where
    kvText :: (Key, AnyValue) -> Text
    kvText (k, AnyValue v) = tab i <> prettyKey k <> " = " <> valText v

    valText :: Value t -> Text
    valText (Bool b)    = Text.toLower $ showText b
    valText (Integer n) = showText n
    valText (Double d)  = showText d
    valText (Text s)    = showText s
    valText (Date d)    = timeText d
    valText (Array a)   = "[" <> Text.intercalate ", " (map valText a) <> "]"

    timeText :: DateTime -> Text
    timeText (Zoned z) = showZoneTime z
    timeText (Local l) = showText l
    timeText (Day d)   = showText d
    timeText (Hours h) = showText h

    showText :: Show a => a -> Text
    showText = Text.pack . show

    showZoneTime :: ZonedTime -> Text
    showZoneTime 
        = Text.pack 
        . (\(x,y) -> x ++ ":" ++ y) 
        . (\z -> splitAt (length z - 2) z)
        . formatTime defaultTimeLocale "%FT%T%Q%z"

-- | Returns pretty formatted tables section of the 'TOML'.
prettyTables :: Int -> Text -> PrefixMap TOML -> Text
prettyTables i pref = Text.concat . map prettyTable . HashMap.elems
  where
    prettyTable :: PrefixTree TOML -> Text
    prettyTable (Leaf k toml) =
        let name = getPref k in
        tab i <> prettyTableName name
              <> prettyTomlInd (succ i) name toml
    prettyTable (Branch k mToml prefMap) =
        let name  = getPref k
            nextI = succ i
            toml  = case mToml of
                        Nothing -> ""
                        Just t  -> prettyTomlInd nextI name t
        in tab i <> prettyTableName name <> toml <> prettyTables nextI name prefMap

    -- Adds next part of the table name to the accumulator.
    getPref :: Key -> Text
    getPref k = case pref of
        "" -> prettyKey k
        _  -> pref <> "." <> prettyKey k

    prettyTableName :: Text -> Text
    prettyTableName n = "[" <> n <> "]"

prettyKey :: Key -> Text
prettyKey (Key k) = Text.intercalate "." $ map unPiece (NonEmpty.toList k)
