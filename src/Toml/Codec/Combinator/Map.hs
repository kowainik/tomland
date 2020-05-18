{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}

{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

TOML-specific combinators for converting between TOML and Haskell Map-like data
types.

There are two way to represent map-like structures with the @tomland@ library.

* Map structure with the key and value represented as key-value pairs:

    @
    foo =
        [ {myKey = "name", myVal = 42}
        , {myKey = "otherName", myVal = 100}
        ]
    @

* Map structure as a table with the @TOML@ key as the map key:

    @
    [foo]
        name = 42
        otherName = 100
    @

You can find both types of the codecs in this module for different map-like
structures. See the following table for the heads up:

+------------------------------+--------------------------------+----------------------------------------------------+
|         Haskell Type         |             @TOML@             |                    'TomlCodec'                     |
+==============================+================================+====================================================+
| __@'Map' 'Int' 'Text'@__     | @x = [{k = 42, v = "foo"}]@    | @'map' ('Toml.int' "k") ('Toml.text' "v") "x"@     |
+------------------------------+--------------------------------+----------------------------------------------------+
| __@'Map' 'Text' 'Int'@__     | @x = {a = 42, b = 11}@         | @'tableMap' 'Toml._KeyText' 'Toml.int' "x"@        |
+------------------------------+--------------------------------+----------------------------------------------------+
| __@'HashMap' 'Int' 'Text'@__ | @x = [{k = 42, v = "foo"}]@    | @'hashMap' ('Toml.int' "k") ('Toml.text' "v") "x"@ |
+------------------------------+--------------------------------+----------------------------------------------------+
| __@'HashMap' 'Text' 'Int'@__ | @x = {a = 42, b = 11}@         | @'tableHashMap' 'Toml._KeyText' 'Toml.int' "x"@    |
+------------------------------+--------------------------------+----------------------------------------------------+
| __@'IntMap' 'Text'@__        | @x = [{k = 42, v = "foo"}]@    | @'intMap' ('Toml.int' "k") ('Toml.text' "v") "x"@  |
+------------------------------+--------------------------------+----------------------------------------------------+
| __@'IntMap' 'Text'@__        | @x = {1 = "one", 2 = "two"}@   | @'tableIntMap' 'Toml._KeyInt' 'Toml.text' "x"@     |
+------------------------------+--------------------------------+----------------------------------------------------+

__Note:__ in case of the missing key on the @TOML@ side an empty map structure
is returned.

@since 1.3.0.0
-}

module Toml.Codec.Combinator.Map
    ( -- * 'Map' codecs
      map
    , tableMap
      -- * 'HashMap' codecs
    , hashMap
    , tableHashMap
      -- * 'IntMap' codecs
    , intMap
    , tableIntMap
    ) where

import Prelude hiding (map)

import Control.Applicative (empty)
import Control.Monad (forM, forM_)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (execState, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)

import Toml.Codec.BiMap (BiMap (..), TomlBiMap)
import Toml.Codec.Code (execTomlCodec)
import Toml.Codec.Combinator.Common (codecReadTOML, whenLeftBiMapError)
import Toml.Codec.Types (Codec (..), TomlCodec, TomlEnv, TomlState)
import Toml.Type.Key (pattern (:||), Key)
import Toml.Type.TOML (TOML (..), insertTable, insertTableArrays)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import qualified Toml.Type.PrefixTree as Prefix


{- | Bidirectional codec for 'Map'. It takes birectional converter for keys and
values and produces bidirectional codec for 'Map'. Currently it works only with array
of tables, so you need to specify 'Map's in TOML files like this:

@
myMap =
    [ { name = "foo", payload = 42 }
    , { name = "bar", payload = 69 }
    ]
@

'TomlCodec' for such TOML field can look like this:

@
Toml.'map' (Toml.'text' "name") (Toml.'int' "payload") "myMap"
@

If there's no key with the name @"myMap"@ then empty 'Map' is returned.

@since 1.2.1.0
-}
map :: forall k v .
       Ord k
    => TomlCodec k  -- ^ Codec for 'Map' keys
    -> TomlCodec v  -- ^ Codec for 'Map' values
    -> Key          -- ^ TOML key where 'Map' is stored
    -> TomlCodec (Map k v)  -- ^ Codec for the 'Map'
map = internalMap Map.empty Map.toList Map.fromList

{- | This 'TomlCodec' helps you to convert TOML key-value pairs
directly to 'Map' using TOML keys as 'Map' keys. It can be convenient
if your 'Map' keys are types like 'Text' or 'Int' and you want to work with raw
TOML keys directly.

For example, if you have TOML like this:

@
[colours]
yellow = "#FFFF00"
red    = { red = 255, green = 0, blue = 0 }
pink   = "#FFC0CB"
@

You want to convert such TOML configuration into the following Haskell
types:


@
__data__ Rgb = Rgb
    { rgbRed   :: Int
    , rgbGreen :: Int
    , rgbBlue  :: Int
    }

__data__ Colour
    = Hex Text
    | RGB Rgb

colourCodec :: 'TomlCodec' Colour
colourCodec = ...

__data__ ColourConfig = ColourConfig
    { configColours :: 'Map' 'Text' Colour
    }
@

And you want in the result to have a 'Map' like this:

@
'Map.fromList'
    [ "yellow" -> Hex "#FFFF00"
    , "pink"   -> Hex "#FFC0CB"
    , "red"    -> Rgb 255 0 0
    ]
@

You can use 'tableMap' to define 'TomlCodec' in the following way:

@
colourConfigCodec :: 'TomlCodec' ColourConfig
colourConfigCodec = ColourConfig
    \<$\> Toml.'tableMap' Toml._KeyText colourCodec "colours" .= configColours
@

__Hint:__ You can use 'Toml.Codec.BiMap._KeyText' or
'Toml.Codec.BiMap._KeyString' to convert betwen TOML keys and 'Map'
keys (or you can write your custom 'TomlBiMap').

__NOTE__: Unlike the 'map' codec, this codec is less flexible (i.e. it doesn't
allow to have arbitrary structures as 'Key's, it works only for
text-like keys), but can be helpful if you want to save a few
keystrokes during TOML configuration. A similar TOML configuration,
but suitable for the 'map' codec will look like this:

@
colours =
    [ { key = "yellow", hex = "#FFFF00" }
    , { key = "pink",   hex = "#FFC0CB" }
    , { key = "red",    rgb = { red = 255, green = 0, blue = 0 } }
    ]
@

@since 1.3.0.0
-}
tableMap
    :: forall k v
    .  Ord k
    => TomlBiMap Key k
    -- ^ Bidirectional converter between TOML 'Key's and 'Map' keys
    -> (Key -> TomlCodec v)
    -- ^ Codec for 'Map' values for the corresponding 'Key'
    -> Key
    -- ^ Table name for 'Map'
    -> TomlCodec (Map k v)
tableMap = internalTableMap Map.empty Map.toList Map.fromList

{- | Bidirectional codec for 'HashMap'. It takes birectional converter for keys and
values and produces bidirectional codec for 'HashMap'. It works with array of
tables, so you need to specify 'HashMap's in TOML files like this:

@
myHashMap =
    [ { name = "foo", payload = 42 }
    , { name = "bar", payload = 69 }
    ]
@

'TomlCodec' for such TOML field can look like this:

@
Toml.'hashMap' (Toml.'text' "name") (Toml.'int' "payload") "myHashMap"
@

If there's no key with the name @"myHashMap"@ then empty 'HashMap' is returned.

@since 1.3.0.0
-}
hashMap
    :: forall k v
    .  (Eq k, Hashable k)
    => TomlCodec k  -- ^ Codec for 'HashMap' keys
    -> TomlCodec v  -- ^ Codec for 'HashMap' values
    -> Key          -- ^ TOML key where 'HashMap' is stored
    -> TomlCodec (HashMap k v)  -- ^ Codec for the 'HashMap'
hashMap = internalMap HashMap.empty HashMap.toList HashMap.fromList

{- | This 'TomlCodec' helps to convert TOML key-value pairs
directly to 'HashMap' using TOML keys as 'HashMap' keys.
It can be convenient if your 'HashMap' keys are types like 'Text' or 'Int' and
you want to work with raw TOML keys directly.

For example, if you can write your 'HashMap' in @TOML@ like this:

@
[myHashMap]
key1 = "value1"
key2 = "value2"
@

@since 1.3.0.0
-}
tableHashMap
    :: forall k v
    .  (Eq k, Hashable k)
    => TomlBiMap Key k
    -- ^ Bidirectional converter between TOML 'Key's and 'HashMap' keys
    -> (Key -> TomlCodec v)
    -- ^ Codec for 'HashMap' values for the corresponding 'Key'
    -> Key
    -- ^ Table name for 'HashMap'
    -> TomlCodec (HashMap k v)
tableHashMap = internalTableMap HashMap.empty HashMap.toList HashMap.fromList

{- | Bidirectional codec for 'IntMap'. It takes birectional converter for keys and
values and produces bidirectional codec for 'IntMap'. It works with array of
tables, so you need to specify 'IntMap's in TOML files like this:

@
myIntMap =
    [ { name = "foo", payload = 42 }
    , { name = "bar", payload = 69 }
    ]
@

'TomlCodec' for such TOML field can look like this:

@
Toml.'intMap' (Toml.'text' "name") (Toml.'int' "payload") "myIntMap"
@

If there's no key with the name @"myIntMap"@ then empty 'IntMap' is returned.

@since 1.3.0.0
-}
intMap
    :: forall v
    .  TomlCodec Int  -- ^ Codec for 'IntMap' keys
    -> TomlCodec v  -- ^ Codec for 'IntMap' values
    -> Key          -- ^ TOML key where 'IntMap' is stored
    -> TomlCodec (IntMap v)  -- ^ Codec for the 'IntMap'
intMap = internalMap IntMap.empty IntMap.toList IntMap.fromList

{- | This 'TomlCodec' helps to convert TOML key-value pairs
directly to 'IntMap' using TOML 'Int' keys as 'IntMap' keys.

For example, if you can write your 'IntMap' in @TOML@ like this:

@
[myIntMap]
1 = "value1"
2 = "value2"
@

@since 1.3.0.0
-}
tableIntMap
    :: forall v
    .  TomlBiMap Key Int
    -- ^ Bidirectional converter between TOML 'Key's and 'IntMap' keys
    -> (Key -> TomlCodec v)
    -- ^ Codec for 'IntMap' values for the corresponding 'Key'
    -> Key
    -- ^ Table name for 'IntMap'
    -> TomlCodec (IntMap v)
tableIntMap = internalTableMap IntMap.empty IntMap.toList IntMap.fromList


----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

internalMap :: forall map k v
    .  map  -- ^ empty map
    -> (map -> [(k, v)])  -- ^ toList function
    -> ([(k, v)] -> map)  -- ^ fromList function
    -> TomlCodec k  -- ^ Codec for Map keys
    -> TomlCodec v  -- ^ Codec for Map values
    -> Key          -- ^ TOML key where Map is stored
    -> TomlCodec map  -- ^ Codec for the Map
internalMap emptyMap toListMap fromListMap keyCodec valCodec key = Codec input output
  where
    input :: TomlEnv map
    input = do
        mTables <- asks $ HashMap.lookup key . tomlTableArrays
        case mTables of
            Nothing -> pure emptyMap
            Just tomls -> fmap fromListMap $ forM (NE.toList tomls) $ \toml -> do
                k <- codecReadTOML toml keyCodec
                v <- codecReadTOML toml valCodec
                pure (k, v)

    output :: map -> TomlState map
    output dict = do
        let tomls = fmap
                (\(k, v) -> execTomlCodec keyCodec k <> execTomlCodec valCodec v)
                (toListMap dict)

        mTables <- gets $ HashMap.lookup key . tomlTableArrays

        let updateAction :: TOML -> TOML
            updateAction = case mTables of
                Nothing -> case tomls of
                    []   -> id
                    t:ts -> insertTableArrays key (t :| ts)
                Just (t :| ts) ->
                    insertTableArrays key $ t :| (ts ++ tomls)

        dict <$ modify updateAction

internalTableMap
    :: forall map k v
    .  map  -- ^ empty map
    -> (map -> [(k, v)])  -- ^ toList function
    -> ([(k, v)] -> map)  -- ^ fromList function
    -> TomlBiMap Key k
    -- ^ Bidirectional converter between TOML 'Key's and Map keys
    -> (Key -> TomlCodec v)
    -- ^ Codec for Map values for the corresponding 'Key'
    -> Key
    -- ^ Table name for Map
    -> TomlCodec map
internalTableMap emptyMap toListMap fromListMap keyBiMap valCodec tableName = Codec input output
  where
    input :: TomlEnv map
    input = asks (Prefix.lookup tableName . tomlTables) >>= \case
        Nothing -> pure emptyMap
        Just toml -> local (const toml) $ do
            valKeys <- asks (HashMap.keys . tomlPairs)
            tableKeys <- asks (fmap (:|| []) . HashMap.keys . tomlTables)
            fmap fromListMap $ forM (valKeys <> tableKeys) $ \key ->
                whenLeftBiMapError (forward keyBiMap key) $ \k ->
                    (k,) <$> codecRead (valCodec key)

    output :: map -> TomlState map
    output m = do
        mTable <- gets $ Prefix.lookup tableName . tomlTables
        let toml = fromMaybe mempty mTable
        let newToml = execState (runMaybeT updateMapTable) toml
        m <$ modify (insertTable tableName newToml)
      where
        updateMapTable :: TomlState ()
        updateMapTable = forM_ (toListMap m) $ \(k, v) -> case backward keyBiMap k of
            Left _    -> empty
            Right key -> codecWrite (valCodec key) v
