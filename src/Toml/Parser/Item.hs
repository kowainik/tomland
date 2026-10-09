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
       , setTableName

       , tomlP
       , keyValP
       ) where

import Control.Applicative (many)
import Data.Foldable (asum)

import Toml.Parser.Core (Parser, eof, lineEndP, scn, text, try, (<?>))
import Toml.Parser.Key (keyP, tableArrayNameP, tableNameP)
import Toml.Parser.Value (valueP)
import Toml.Type.Key (Key)
import Toml.Type.UValue (UValue)


{- | One item of a TOML file. It could be either:

* A name of a table
* A name of a table array
* Key-value pair, where the value is not yet validated: it may be an inline
  table or an array of inline tables

Knowing a list of 'TomlItem's, it's possible to construct 'Toml.Type.TOML.TOML'
from this information.

@since 1.4.0.0: 'KeyVal' holds an untyped 'UValue'; the @InlineTable@ and
@InlineTableArray@ constructors are gone.
-}
data TomlItem
    = TableName !Key
    | TableArrayName !Key
    | KeyVal !Key !UValue
    deriving stock (Show, Eq)

{- | Changes name of table to a new one. Works only for 'TableName' and
'TableArrayName' constructors.
-}
setTableName :: Key -> TomlItem -> TomlItem
setTableName new = \case
    TableName _ -> TableName new
    TableArrayName _ -> TableArrayName new
    item -> item

----------------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------------

-- | Parser for a single item in the TOML file.
tomlItemP :: Parser TomlItem
tomlItemP = asum
    [ TableName <$> try tableNameP <?> "table name"
    , TableArrayName <$> tableArrayNameP <?> "array of tables name"
    , keyValP
    ]

-- | Parser for @key = value@ pairs.
keyValP :: Parser TomlItem
keyValP = KeyVal <$> (keyP <* text "=") <*> valueP <?> "key-value pair"

{- | Parser for the full content of the .toml file. Every item must be
followed by a newline (or the end of input); only comments may follow an item
on the same line.
-}
tomlP :: Parser [TomlItem]
tomlP = scn *> many (tomlItemP <* lineEndP) <* eof
