module Toml.Printer
       ( toml2Text
       ) where

import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.Text (Text)

import Toml.Type (DateTime (..), Key (..), TOML (..), TableId (..), Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

tab :: Int -> Text
tab n = Text.cons '\n' (Text.replicate (2*n) " ")

toml2Text :: TOML -> Text
toml2Text = toml2TextInd 0

toml2TextInd :: Int -> TOML -> Text
toml2TextInd i TOML{..} = keyValue2Text i tomlPairs  <> "\n"
                       <> tables2Text   i tomlTables

keyValue2Text :: Int -> HashMap Key Value -> Text
keyValue2Text i = Text.concat . map kvText . HashMap.toList
  where
    kvText :: (Key, Value) -> Text
    kvText (Key{..}, v) = tab i <> unKey <> " = " <> valText v

    valText :: Value -> Text
    valText (Bool b)   = Text.toLower $ showText b
    valText (Int n)    = showText n
    valText (Float d)  = showText d
    valText (String s) = showText s
    valText (Date d)   = timeText d
    valText (Array a)  = "[" <> Text.intercalate ", " (map valText a) <> "]"

    timeText :: DateTime -> Text
    timeText (Zoned z) = showText z
    timeText (Local l) = showText l
    timeText (Day d)   = showText d
    timeText (Hours h) = showText h

    showText :: Show a => a -> Text
    showText = Text.pack . show

tables2Text :: Int -> HashMap TableId TOML -> Text
tables2Text i = Text.concat . map table2Text . HashMap.toList
  where
    table2Text :: (TableId, TOML) -> Text
    table2Text (tn, toml) = tab i <> tableName2Text tn
                                  <> toml2TextInd (succ i) toml

    tableName2Text :: TableId -> Text
    tableName2Text TableId{..} = "[" <> Text.intercalate "." (NonEmpty.toList unTableId) <> "]"
