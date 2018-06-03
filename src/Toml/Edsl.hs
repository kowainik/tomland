module Toml.Edsl
       ( toml
       , (=:)
       , table
       ) where

import Control.Monad.State (State, execState, modify)

import Toml.PrefixTree (Key)
import Toml.Type (AnyValue (..), TOML (..), Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Toml.PrefixTree as Prefix

type Env = State TOML ()

toml :: Env -> TOML
toml env = execState env $ TOML mempty mempty

(=:) :: Key -> Value a -> Env
(=:) k v = modify $ \tml -> tml {tomlPairs = HashMap.insert k (AnyValue v) (tomlPairs tml)}

table :: Key -> Env -> Env
table k env = modify $ \tml -> tml
    { tomlTables = Prefix.insert k (execState env (TOML mempty mempty)) (tomlTables tml)
    }
