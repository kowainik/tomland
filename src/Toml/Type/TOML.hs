module Toml.Type.TOML
       ( TOML (..)
       , insertKeyVal
       , insertKeyAnyVal
       , insertTable
       ) where

import Data.HashMap.Strict (HashMap)
import Data.Semigroup (Semigroup (..))

import Toml.PrefixTree (Key (..), PrefixMap)
import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Value (Value)

import qualified Data.HashMap.Strict as HashMap
import qualified Toml.PrefixTree as Prefix


-- TODO: describe how some TOML document will look like with this type
{- | Represents TOML configuration value. -}
data TOML = TOML
    { tomlPairs  :: HashMap Key AnyValue
    , tomlTables :: PrefixMap TOML
    -- tomlTableArrays :: HashMap Key (NonEmpty TOML)
    } deriving (Show, Eq)

instance Semigroup TOML where
    (TOML pairsA tablesA) <> (TOML pairsB tablesB) = TOML
        (pairsA <> pairsB)
        (HashMap.unionWith (<>) tablesA tablesB)

instance Monoid TOML where
    mappend = (<>)
    mempty = TOML mempty mempty

-- | Inserts given key-value into the 'TOML'.
insertKeyVal :: Key -> Value a -> TOML -> TOML
insertKeyVal k v = insertKeyAnyVal k (AnyValue v)

-- | Inserts given key-value into the 'TOML'.
insertKeyAnyVal :: Key -> AnyValue -> TOML -> TOML
insertKeyAnyVal k av toml = toml { tomlPairs = HashMap.insert k av (tomlPairs toml) }

-- | Inserts given table into the 'TOML'.
insertTable :: Key -> TOML -> TOML -> TOML
insertTable k inToml toml = toml
    { tomlTables = Prefix.insert k inToml (tomlTables toml)
    }
