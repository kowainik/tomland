{- | This module introduces EDSL for manually specifying 'TOML' data types.

__Example:__

@
exampleToml :: TOML
exampleToml = mkToml $ do
    \"key1\" =: 1
    \"key2\" =: Bool True
    table \"tableName\" $
        \"tableKey\" =: Array [\"Oh\", \"Hi\", \"Mark\"]
    array \"arrayName\" $
        \"elem1\" =: \"yes\" :|
        [ table \"elem2\" $ \"deep\" =: Integer 7
        , empty
        ]
@

-}

module Toml.Edsl
       ( mkToml
       , empty
       , (=:)
       , table
       , array
       ) where

import Control.Monad.State (State, execState, modify, put)
import Data.List.NonEmpty (NonEmpty)

import Toml.PrefixTree (Key)
import Toml.Type (TOML (..), Value, insertKeyVal, insertTable, insertTableArrays)


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

-- | Adds table to the 'TDSL'.
table :: Key -> TDSL -> TDSL
table k = modify . insertTable k . mkToml

-- | Adds array of tables to the 'TDSL'.
array :: Key -> NonEmpty TDSL -> TDSL
array k = modify . insertTableArrays k . fmap mkToml
