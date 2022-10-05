{- |
Module                  : Toml.Type
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Core types for TOML AST.
-}

module Toml.Type
    ( -- $toml
      module Toml.Type.TOML

--      -- $edsl
--    , module Toml.Type.Edsl
--    Edsl.table conflicts with Codec.table

      -- $printer
    , module Toml.Type.Printer
      -- $prefix
    , module Toml.Type.PrefixTree
      -- $key
    , module Toml.Type.Key
      -- $uvalue
    , module Toml.Type.UValue
      -- $value
    , module Toml.Type.Value
      -- $anyvalue
    , module Toml.Type.AnyValue
    ) where

import Toml.Type.AnyValue
-- import Toml.Type.Edsl
import Toml.Type.Key
import Toml.Type.PrefixTree
import Toml.Type.Printer
import Toml.Type.TOML
import Toml.Type.UValue
import Toml.Type.Value

{- $toml
Main TOML AST data type. Text is converted to 'TOML' first. All codecs
work with the 'TOML' type instead of raw text.
-}

-- {- $eDSL
-- eDSL for type-safe construction of the 'TOML' values.
-- -}

{- $printer
Pretty-printer for 'TOML'.
-}

{- $prefix
'PrefixMap' and 'PrefixTree' types that help representing 'TOML' AST.
-}

{- $key
Key in key-value pairs and table names. Also 'Key' in 'PrefixMap'.
-}

{- $uvalue
Untyped value obtained directly from parsing.
-}

{- $value
Typed 'Value'. Result of type-checking 'UValue'.
-}

{- $anyvalue
Existential wrapper around 'Value' to be able to store 'Value's of
different types inside lists or other containers like
'Data.Map.Strict.Map' or 'Data.HashMap.Strict.HashMap'.
-}
