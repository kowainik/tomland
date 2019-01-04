{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}

-- | Contains TOML-specific combinators for converting between TOML and user data types.

module Toml.Bi.Combinators
       ( -- * Toml codecs
         bool
       , int
       , integer
       , natural
       , word
       , double
       , float
       , text
       , textBy
       , read
       , string
       , byteString
       , lazyByteString
       , zonedTime
       , localTime
       , day
       , timeOfDay
       , arrayOf
       , arraySetOf
       , arrayIntSet
       , arrayHashSetOf
       , arrayNonEmptyOf

         -- * Combinators
       , match
       , table
       ) where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (execState, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Word (Word)
import Numeric.Natural (Natural)

import Toml.Bi.Code (DecodeException (..), Env, St, TomlCodec)
import Toml.Bi.Monad (Codec (..))
import Toml.BiMap (BiMap (..), TomlBiMap, _Array, _Bool, _ByteString, _Day, _Double, _Float,
                   _HashSet, _Int, _IntSet, _Integer, _LByteString, _LocalTime, _Natural, _NonEmpty,
                   _Read, _Set, _String, _Text, _TextBy, _TimeOfDay, _Word, _ZonedTime)

import Toml.PrefixTree (Key)
import Toml.Type (AnyValue (..), TOML (..), insertKeyAnyVal, insertTable)

import Prelude hiding (read)

import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap
import qualified Toml.PrefixTree as Prefix

----------------------------------------------------------------------------
-- Generalized versions of parsers
----------------------------------------------------------------------------

{- | General function to create bidirectional converters for values.
-}
match :: forall a . TomlBiMap a AnyValue -> Key -> TomlCodec a
match BiMap{..} key = Codec input output
  where
    input :: Env a
    input = do
        mVal <- asks $ HashMap.lookup key . tomlPairs
        case mVal of
            Nothing -> throwError $ KeyNotFound key
            Just anyVal@(AnyValue _) -> case backward anyVal of
                Right v  -> pure v
                Left err -> throwError $ BiMapError err

    output :: a -> St a
    output a = do
        anyVal <- MaybeT $ pure $ either (const Nothing) Just $ forward a
        a <$ modify (insertKeyAnyVal key anyVal)

----------------------------------------------------------------------------
-- Toml parsers
----------------------------------------------------------------------------

-- | Parser for boolean values.
bool :: Key -> TomlCodec Bool
bool = match _Bool

-- | Parser for integer values.
integer :: Key -> TomlCodec Integer
integer = match _Integer

-- | Parser for integer values.
int :: Key -> TomlCodec Int
int = match _Int

-- | Parser for natural values.
natural :: Key -> TomlCodec Natural
natural = match _Natural

-- | Parser for word values.
word :: Key -> TomlCodec Word
word = match _Word

-- | Parser for floating point values as double.
double :: Key -> TomlCodec Double
double = match _Double

-- | Parser for floating point values as float.
float :: Key -> TomlCodec Float
float = match _Float

-- | Parser for string values as text.
text :: Key -> TomlCodec Text
text = match _Text

-- | Parser for values as text with custom functions.
textBy :: (a -> Text) -> (Text -> Either Text a) -> Key -> TomlCodec a
textBy to from = match (_TextBy to from)

-- | Parser for string values as string.
string :: Key -> TomlCodec String
string = match _String

-- | Parser for values with a `Read` and `Show` instance.
read :: (Show a, Read a) => Key -> TomlCodec a
read = match _Read

-- | Parser for byte vectors values as strict bytestring.
byteString :: Key -> TomlCodec ByteString
byteString = match _ByteString

-- | Parser for byte vectors values as lazy bytestring.
lazyByteString :: Key -> TomlCodec BL.ByteString
lazyByteString = match _LByteString

-- | Parser for zoned time values.
zonedTime :: Key -> TomlCodec ZonedTime
zonedTime = match _ZonedTime

-- | Parser for local time values.
localTime :: Key -> TomlCodec LocalTime
localTime = match _LocalTime

-- | Parser for day values.
day :: Key -> TomlCodec Day
day = match _Day

-- | Parser for time of day values.
timeOfDay :: Key -> TomlCodec TimeOfDay
timeOfDay = match _TimeOfDay

-- | Parser for list of values. Takes converter for single value and
-- returns a list of values.
arrayOf :: TomlBiMap a AnyValue -> Key -> TomlCodec [a]
arrayOf = match . _Array

-- | Parser for sets. Takes converter for single value and
-- returns a set of values.
arraySetOf :: Ord a => TomlBiMap a AnyValue -> Key -> TomlCodec (Set a)
arraySetOf = match . _Set

-- | Parser for sets of ints. Takes converter for single value and
-- returns a set of ints.
arrayIntSet :: Key -> TomlCodec IntSet
arrayIntSet = match _IntSet

-- | Parser for hash sets. Takes converter for single hashable value and
-- returns a set of hashable values.
arrayHashSetOf
    :: (Hashable a, Eq a)
    => TomlBiMap a AnyValue
    -> Key
    -> TomlCodec (HashSet a)
arrayHashSetOf = match . _HashSet

-- | Parser for non- empty lists of values. Takes converter for single value and
-- returns a non-empty list of values.
arrayNonEmptyOf :: TomlBiMap a AnyValue -> Key -> TomlCodec (NonEmpty a)
arrayNonEmptyOf = match . _NonEmpty

-- | Parser for tables. Use it when when you have nested objects.
table :: forall a . TomlCodec a -> Key -> TomlCodec a
table codec key = Codec input output
  where
    input :: Env a
    input = do
        mTable <- asks $ Prefix.lookup key . tomlTables
        case mTable of
            Nothing   -> throwError $ TableNotFound key
            Just toml -> local (const toml) (codecRead codec) `catchError` handleTableName

    output :: a -> St a
    output a = do
        mTable <- gets $ Prefix.lookup key . tomlTables
        let toml = fromMaybe mempty mTable
        let newToml = execState (runMaybeT $ codecWrite codec a) toml
        a <$ modify (insertTable key newToml)

    handleTableName :: DecodeException -> Env a
    handleTableName (KeyNotFound name)        = throwError $ KeyNotFound (key <> name)
    handleTableName (TableNotFound name)      = throwError $ TableNotFound (key <> name)
    handleTableName (TypeMismatch name t1 t2) = throwError $ TypeMismatch (key <> name) t1 t2
    handleTableName e                         = throwError e
