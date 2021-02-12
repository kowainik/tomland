{-# LANGUAGE DeriveAnyClass #-}

{- |
Module                  : Toml.Type.TOML
Copyright               : (c) 2018-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Type of TOML AST. This is intermediate representation of TOML parsed from text.
-}

module Toml.Type.TOML
       ( TOML (..)
       , insertKeyVal
       , insertKeyAnyVal
       , insertTable
       , insertTableArrays

       , tomlDiff
       ) where

import Control.DeepSeq (NFData)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Key (Key (..))
import Toml.Type.PrefixTree (PrefixMap)
import Toml.Type.Value (Value)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import qualified Toml.Type.PrefixTree as Prefix


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

@since 0.0.0
-}
data TOML = TOML
    { tomlPairs       :: !(HashMap Key AnyValue)
    , tomlTables      :: !(PrefixMap TOML)
    , tomlTableArrays :: !(HashMap Key (NonEmpty TOML))
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (NFData)

-- | @since 0.3
instance Semigroup TOML where
    (<>) :: TOML -> TOML -> TOML
    TOML pairsA tablesA arraysA <> TOML pairsB tablesB arraysB = TOML
        (pairsA <> pairsB)
        (HashMap.unionWith (<>) tablesA tablesB)
        (arraysA <> arraysB)
    {-# INLINE (<>) #-}

-- | @since 0.3
instance Monoid TOML where
    mempty :: TOML
    mempty = TOML mempty mempty mempty
    {-# INLINE mempty #-}

    mappend :: TOML -> TOML -> TOML
    mappend = (<>)
    {-# INLINE mappend #-}

-- | Inserts given key-value into the 'TOML'.
insertKeyVal :: Key -> Value a -> TOML -> TOML
insertKeyVal k v = insertKeyAnyVal k (AnyValue v)
{-# INLINE insertKeyVal #-}

-- | Inserts given key-value into the 'TOML'.
insertKeyAnyVal :: Key -> AnyValue -> TOML -> TOML
insertKeyAnyVal k av toml = toml { tomlPairs = HashMap.insert k av (tomlPairs toml) }
{-# INLINE insertKeyAnyVal #-}

-- | Inserts given table into the 'TOML'.
insertTable :: Key -> TOML -> TOML -> TOML
insertTable k inToml toml = toml
    { tomlTables = Prefix.insert k inToml (tomlTables toml)
    }
{-# INLINE insertTable #-}

-- | Inserts given array of tables into the 'TOML'.
insertTableArrays :: Key -> NonEmpty TOML -> TOML -> TOML
insertTableArrays k arr toml = toml
    { tomlTableArrays = HashMap.insert k arr (tomlTableArrays toml)
    }
{-# INLINE insertTableArrays #-}

{- | Difference of two 'TOML's. Returns elements of the first 'TOML' that are
not existing in the second one.

@since x.x.x.x
-}
tomlDiff :: TOML -> TOML -> TOML
tomlDiff t1 t2 = TOML
    { tomlPairs = HashMap.difference (tomlPairs t1) (tomlPairs t2)
    , tomlTables = prefixMapDiff (tomlTables t1) (tomlTables t2)
    , tomlTableArrays = HashMap.differenceWith interTomlsDiff
        (tomlTableArrays t1)
        (tomlTableArrays t2)
    }
  where
    interTomlsDiff :: NonEmpty TOML -> NonEmpty TOML -> Maybe (NonEmpty TOML)
    interTomlsDiff tl1 tl2 = NE.nonEmpty $ tomlListDiff (NE.toList tl1) (NE.toList tl2)
{-# INLINE tomlDiff #-}

{- | Difference of two 'PrefixMap's. Returns elements of the first 'PrefixMap'
that are not existing in the second one.

@since x.x.x.x
-}
prefixMapDiff :: PrefixMap TOML -> PrefixMap TOML -> PrefixMap TOML
prefixMapDiff = Prefix.differenceWith $ \toml1 toml2 -> let diff = tomlDiff toml1 toml2 in
    if diff == mempty
    then Nothing
    else Just diff


tomlListDiff :: [TOML] -> [TOML] -> [TOML]
tomlListDiff [] _ = []
tomlListDiff ts [] = ts
tomlListDiff (t1:t1s) (t2:t2s) = let diff = tomlDiff t1 t2 in
    if diff == mempty
    then tomlListDiff t1s t2s
    else diff : tomlListDiff t1s t2s
