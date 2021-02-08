{- |
Module                  : Toml.Type.Edsl
Copyright               : (c) 2018-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module introduces EDSL for manually specifying 'TOML' data types.

Consider the following raw TOML:

@
key1 = 1
key2 = true

[meme-quotes]
  quote1 = [ \"Oh\", \"Hi\", \"Mark\" ]

[[arrayName]]
  elem1 = "yes"

[[arrayName]]
  [arrayName.elem2]
    deep = 7

[[arrayName]]
@

using functions from this module you can specify the above TOML in safer way:

@
exampleToml :: 'TOML'
exampleToml = 'mkToml' $ __do__
    \"key1\" '=:' 1
    \"key2\" '=:' Bool True
    'table' \"meme-quotes\" $
        \"quote1\" '=:' Array [\"Oh\", \"Hi\", \"Mark\"]
    'tableArray' \"arrayName\" $
        \"elem1\" '=:' \"yes\" :|
        [ 'table' \"elem2\" $ \"deep\" '=:' Integer 7
        , 'empty'
        ]
@

@since 0.3
-}

module Toml.Type.Edsl
       ( TDSL
       , mkToml
       , empty
       , (=:)
       , table
       , tableArray
       ) where

import Control.Monad.State (State, execState, modify, put)
import Data.List.NonEmpty (NonEmpty)

import Toml.Type.Key (Key)
import Toml.Type.TOML (TOML (..), insertKeyVal, insertTable, insertTableArrays)
import Toml.Type.Value (Value)


{- | Monad for creating TOML.

@since 0.3
-}
type TDSL = State TOML ()

{- | Creates 'TOML' from the 'TDSL'.

@since 0.3
-}
mkToml :: TDSL -> TOML
mkToml env = execState env mempty
{-# INLINE mkToml #-}

{- | Creates an empty 'TDSL'.

@since 0.3
-}
empty :: TDSL
empty = put mempty
{-# INLINE empty #-}

{- | Adds key-value pair to the 'TDSL'.

@since 0.3
-}
(=:) :: Key -> Value a -> TDSL
(=:) k v = modify $ insertKeyVal k v
{-# INLINE (=:) #-}

{- | Adds table to the 'TDSL'.

@since 0.3
-}
table :: Key -> TDSL -> TDSL
table k = modify . insertTable k . mkToml
{-# INLINE table #-}

{- | Adds array of tables to the 'TDSL'.

@since 1.0.0
-}
tableArray :: Key -> NonEmpty TDSL -> TDSL
tableArray k = modify . insertTableArrays k . fmap mkToml
{-# INLINE tableArray #-}
