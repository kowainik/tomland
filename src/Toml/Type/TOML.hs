{-# LANGUAGE DeriveAnyClass #-}

-- | Type of TOML AST. This is intermediate representation of TOML parsed from text.

module Toml.Type.TOML
       ( TOML (..)
       , insertKeyVal
       , insertKeyAnyVal
       , insertTable
       , insertTableArrays
       ) where

import Control.DeepSeq (NFData)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (Semigroup (..))
import GHC.Generics (Generic)

import Toml.PrefixTree (Key (..), PrefixMap)
import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Value (Value)

import qualified Data.HashMap.Strict as HashMap
import qualified Toml.PrefixTree as Prefix


{- | Represents TOML configuration value.

For example, if we have the following @TOML@ file:

@
server.port        = 8080
server.codes       = [ 5, 10, 42 ]
server.description = "This is production server."

[mail]
    host = "smtp.gmail.com"
    send-if-inactive = false

[[user]]
    id = 42

[[user]]
    name = "Foo Bar"
@

corresponding 'TOML' looks like:

@
TOML
    { tomlPairs = fromList
        [ ( "server" :| [ "port" ] , Integer 8080)
        , ( "server" :| [ "codes" ] , Array [ Integer 5 , Integer 10 , Integer 42])
        , ( "server" :| [ "description" ] , Text "This is production server.")
        ]
    , tomlTables = fromList
        [ ( "mail"
          , Leaf ( "mail" :| [] )
              ( TOML
                  { tomlPairs = fromList
                      [ ( "host" :| [] , Text "smtp.gmail.com")
                      , ( "send-if-inactive" :| [] , Bool False)
                      ]
                  , tomlTables = fromList []
                  , tomlTableArrays = fromList []
                  }
              )
          )
        ]
    , tomlTableArrays = fromList
        [ ( "user" :| []
          , TOML
              { tomlPairs = fromList [( "id" :| [] , Integer 42)]
              , tomlTables = fromList []
              , tomlTableArrays = fromList []
              } :|
              [ TOML
                  { tomlPairs = fromList [( "name" :| [] , Text "Foo Bar")]
                  , tomlTables = fromList []
                  , tomlTableArrays = fromList []
                  }
              ]
          )
        ]
    }
@
-}
data TOML = TOML
    { tomlPairs       :: HashMap Key AnyValue
    , tomlTables      :: PrefixMap TOML
    , tomlTableArrays :: HashMap Key (NonEmpty TOML)
    } deriving (Show, Eq, NFData, Generic)

instance Semigroup TOML where
    (TOML pairsA tablesA arraysA) <> (TOML pairsB tablesB arraysB) = TOML
        (pairsA <> pairsB)
        (HashMap.unionWith (<>) tablesA tablesB)
        (arraysA <> arraysB)

instance Monoid TOML where
    mappend = (<>)
    mempty = TOML mempty mempty mempty

-- | Inserts given key-value into the 'TOML'.
insertKeyVal :: Key -> Value a -> TOML -> TOML
insertKeyVal k v = insertKeyAnyVal k (AnyValue v)

-- | Inserts given key-value into the 'TOML'.
insertKeyAnyVal :: Key -> AnyValue -> TOML -> TOML
insertKeyAnyVal k av toml =toml { tomlPairs = HashMap.insert k av (tomlPairs toml) }

-- | Inserts given table into the 'TOML'.
insertTable :: Key -> TOML -> TOML -> TOML
insertTable k inToml toml = toml
    { tomlTables = Prefix.insert k inToml (tomlTables toml)
    }

-- | Inserts given array of tables into the 'TOML'.
insertTableArrays :: Key -> NonEmpty TOML -> TOML -> TOML
insertTableArrays k arr toml = toml
    { tomlTableArrays = HashMap.insert k arr (tomlTableArrays toml)
    }
