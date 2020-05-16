{-|
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains the definition of the 'TomlItem' data type which
represents either key-value pair or table name. This data type serves the
purpose to be the intermediate representation of parsing a TOML file which will
be assembled to TOML AST later.

@since 1.2.0.0
-}

module Toml.Parser.Item
       ( TomlItem (..)
       , Table (..)
       , setTableName

       , tomlP
       , keyValP
       ) where

import Control.Applicative (liftA2, many)
import Control.Applicative.Combinators.NonEmpty (sepEndBy1)
import Control.Monad.Combinators (between, sepEndBy)
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty)

import Toml.Parser.Core (Parser, eof, sc, text, try, (<?>))
import Toml.Parser.Key (keyP, tableArrayNameP, tableNameP)
import Toml.Parser.Value (anyValueP)
import Toml.Type.AnyValue (AnyValue)
import Toml.Type.Key (Key)


{- | One item of a TOML file. It could be either:

* A name of a table
* A name of a table array
* Key-value pair
* Inline table
* Inline array of tables

Knowing a list of 'TomlItem's, it's possible to construct 'Toml.Type.TOML.TOML'
from this information.
-}
data TomlItem
    = TableName !Key
    | TableArrayName !Key
    | KeyVal !Key !AnyValue
    | InlineTable !Key !Table
    | InlineTableArray !Key !(NonEmpty Table)
    deriving stock (Show, Eq)

{- | Changes name of table to a new one. Works only for 'TableName' and
'TableArrayName' constructors.
-}
setTableName :: Key -> TomlItem -> TomlItem
setTableName new = \case
    TableName _ -> TableName new
    TableArrayName _ -> TableArrayName new
    item -> item

{- | Table that contains only @key = val@ pairs.
-}
newtype Table = Table
    { unTable :: [(Key, AnyValue)]
    } deriving stock (Show)
      deriving newtype (Eq)

----------------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------------

-- | Parser for inline tables.
inlineTableP :: Parser Table
inlineTableP =
    fmap Table
    $ between (text "{") (text "}")
    $ liftA2 (,) (keyP <* text "=") anyValueP `sepEndBy` text ","

-- | Parser for inline arrays of tables.
inlineTableArrayP :: Parser (NonEmpty Table)
inlineTableArrayP = between (text "[") (text "]")
    $ inlineTableP `sepEndBy1` text ","

-- | Parser for a single item in the TOML file.
tomlItemP :: Parser TomlItem
tomlItemP = asum
    [ TableName <$> try tableNameP <?> "table name"
    , TableArrayName <$> tableArrayNameP <?> "array of tables name"
    , keyValP
    ]

{- | parser for @"key = val"@ pairs; can be one of three forms:

1. key = { ... }
2. key = [ {...}, {...}, ... ]
3. key = ...
-}
keyValP :: Parser TomlItem
keyValP = do
    key <- keyP <* text "="
    asum
        [ InlineTable key <$> inlineTableP <?> "inline table"
        , InlineTableArray key <$> try inlineTableArrayP <?> "inline array of tables"
        , KeyVal key <$> anyValueP <?> "key-value pair"
        ]

-- | Parser for the full content of the .toml file.
tomlP :: Parser [TomlItem]
tomlP = sc *> many tomlItemP <* eof
