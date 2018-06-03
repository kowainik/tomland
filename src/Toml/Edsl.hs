{- | This module introduces EDSL for manually specifying 'TOML' data types.

__Example:__

@
exampleToml :: TOML
exampleToml = mkToml $ do
    "key1" =: 1
    "key2" =: Bool True
    table "tableName" $
        "tableKey" =: Array ["Oh", "Hi", "Mark"]
@

-}

module Toml.Edsl
       ( mkToml
       , (=:)
       , table
       ) where

import Control.Monad.State (State, execState, modify)

import Toml.PrefixTree (Key)
import Toml.Type (TOML (..), Value, insertKeyVal, insertTable)


type TDSL = State TOML ()

-- | Creates 'TOML' from the 'TDSL'.
mkToml :: TDSL -> TOML
mkToml env = execState env mempty

-- | Adds key-value pair to the 'TDSL'.
(=:) :: Key -> Value a -> TDSL
(=:) k v = modify $ insertKeyVal k v

-- | Adds table to the 'TDSL'.
table :: Key -> TDSL -> TDSL
table k = modify . insertTable k . mkToml
