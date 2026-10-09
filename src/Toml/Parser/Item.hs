{- |
Module                  : Toml.Parser.Item
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

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

import Control.Applicative (many)
import Control.Applicative.Combinators.NonEmpty (sepEndBy1)
import Control.Monad.Combinators (between, sepEndBy)
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty)

import Toml.Parser.Core (Parser, eof, lineEndP, scn, text, try, (<?>))
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

{- | Contents of an inline table: a list of @key = val@ pairs, where a value can
itself be an inline table or an array of inline tables. Only the 'KeyVal',
'InlineTable' and 'InlineTableArray' constructors of 'TomlItem' appear here.

@since 1.4.0.0
-}
newtype Table = Table
    { unTable :: [TomlItem]
    } deriving stock (Show)
      deriving newtype (Eq)

----------------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------------

{- | Parser for inline tables. Newlines and comments are allowed between the
key-value pairs, and a trailing comma is allowed after the last one.
-}
inlineTableP :: Parser Table
inlineTableP =
    fmap Table
    $ between (text "{" *> scn) (text "}")
    $ (keyValP <* scn) `sepEndBy` (text "," *> scn)

-- | Parser for inline arrays of tables.
inlineTableArrayP :: Parser (NonEmpty Table)
inlineTableArrayP = between (text "[" *> scn) (text "]")
    $ (inlineTableP <* scn) `sepEndBy1` (text "," *> scn)

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

{- | Parser for the full content of the .toml file. Every item must be
followed by a newline (or the end of input); only comments may follow an item
on the same line.
-}
tomlP :: Parser [TomlItem]
tomlP = scn *> many (tomlItemP <* lineEndP) <* eof
