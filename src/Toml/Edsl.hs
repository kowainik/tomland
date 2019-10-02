{- | This module introduces EDSL for manually specifying 'TOML' data types.

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
-}

module Toml.Edsl
       ( TDSL
       , mkToml
       , empty
       , (=:)
       , table
       , tableArray
       ) where

import Control.Monad.State (State, execState, modify, put)
import Data.List.NonEmpty (NonEmpty)

import Toml.PrefixTree (Key)
import Toml.Type (TOML (..), Value, insertKeyVal, insertTable, insertTableArrays)


-- | Monad for creating TOML.
type TDSL = State TOML ()

-- | Creates 'TOML' from the 'TDSL'.
mkToml :: TDSL -> TOML
mkToml env = execState env mempty

-- | Creates an empty 'TDSL'.
empty :: TDSL
empty = put mempty

-- | Adds key-value pair to the 'TDSL'.
(=:) :: Key -> Value a -> TDSL
(=:) k v = modify $ insertKeyVal k v
{-# INLINE (=:) #-}

-- | Adds table to the 'TDSL'.
table :: Key -> TDSL -> TDSL
table k = modify . insertTable k . mkToml

-- | Adds array of tables to the 'TDSL'.
tableArray :: Key -> NonEmpty TDSL -> TDSL
tableArray k = modify . insertTableArrays k . fmap mkToml
