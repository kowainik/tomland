{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}

-- | Contains TOML-specific combinators for converting between TOML and user data types.

module Toml.Bi.Combinators
       ( -- * Basic codecs for primitive values
         -- ** Boolean
         bool
         -- ** Integral numbers
       , integer
       , natural
       , int
       , word
       , word8
         -- ** Floating point numbers
       , double
       , float
         -- ** Text types
       , text
       , lazyText
       , byteString
       , lazyByteString
       , byteStringArray
       , lazyByteStringArray
       , string
         -- ** Time types
       , zonedTime
       , localTime
       , day
       , timeOfDay

         -- * Codecs for containers of primitives
       , arrayOf
       , arraySetOf
       , arrayIntSet
       , arrayHashSetOf
       , arrayNonEmptyOf

         -- * Additional codecs for custom types
       , textBy
       , read
       , enumBounded

         -- * Combinators for tables
       , table
       , nonEmpty
       , list
       , set
       , hashSet

         -- * General construction of codecs
       , match
       ) where

import Prelude hiding (read)

import Control.Monad (forM)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (execState, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Toml.Bi.Code (DecodeException (..), Env, St, TomlCodec, execTomlCodec)
import Toml.Bi.Map (BiMap (..), TomlBiMap, _Array, _Bool, _ByteString, _ByteStringArray, _Day,
                    _Double, _EnumBounded, _Float, _HashSet, _Int, _IntSet, _Integer, _LByteString,
                    _LByteStringArray, _LText, _LocalTime, _Natural, _NonEmpty, _Read, _Set,
                    _String, _Text, _TextBy, _TimeOfDay, _Word, _Word8, _ZonedTime)
import Toml.Bi.Monad (Codec (..), dimap)
import Toml.PrefixTree (Key)
import Toml.Type (AnyValue (..), TOML (..), insertKeyAnyVal, insertTable, insertTableArrays)

import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HS
import qualified Data.Set as S
import qualified Data.Text.Lazy as L
import qualified Toml.PrefixTree as Prefix

{- | General function to create bidirectional converters for key-value pairs. In
order to use this function you need to create 'TomlBiMap' for your type and
'AnyValue':

@
_MyType :: 'TomlBiMap' MyType 'AnyValue'
@

And then you can create codec for your type using 'match' function:

@
myType :: 'Key' -> 'TomlCodec' MyType
myType = 'match' _MyType
@
-}
match :: forall a . TomlBiMap a AnyValue -> Key -> TomlCodec a
match BiMap{..} key = Codec input output
  where
    input :: Env a
    input = do
        mVal <- asks $ HashMap.lookup key . tomlPairs
        case mVal of
            Nothing -> throwError $ KeyNotFound key
            Just anyVal -> case backward anyVal of
                Right v  -> pure v
                Left err -> throwError $ BiMapError err

    output :: a -> St a
    output a = do
        anyVal <- MaybeT $ pure $ either (const Nothing) Just $ forward a
        a <$ modify (insertKeyAnyVal key anyVal)

-- | Codec for boolean values.
bool :: Key -> TomlCodec Bool
bool = match _Bool
{-# INLINE bool #-}

-- | Codec for integer values.
integer :: Key -> TomlCodec Integer
integer = match _Integer
{-# INLINE integer #-}

-- | Codec for integer values.
int :: Key -> TomlCodec Int
int = match _Int
{-# INLINE int #-}

-- | Codec for natural values.
natural :: Key -> TomlCodec Natural
natural = match _Natural
{-# INLINE natural #-}

-- | Codec for word values.
word :: Key -> TomlCodec Word
word = match _Word
{-# INLINE word #-}

-- | Codec for word8 values.
word8 :: Key -> TomlCodec Word8
word8 = match _Word8
{-# INLINE word8 #-}

-- | Codec for floating point values with double precision.
double :: Key -> TomlCodec Double
double = match _Double
{-# INLINE double #-}

-- | Codec for floating point values.
float :: Key -> TomlCodec Float
float = match _Float
{-# INLINE float #-}

-- | Codec for text values.
text :: Key -> TomlCodec Text
text = match _Text
{-# INLINE text #-}

-- | Codec for lazy text values.
lazyText :: Key -> TomlCodec L.Text
lazyText = match _LText
{-# INLINE lazyText #-}

-- | Codec for text values with custom error messages for parsing.
textBy :: (a -> Text) -> (Text -> Either Text a) -> Key -> TomlCodec a
textBy to from = match (_TextBy to from)
{-# INLINE textBy #-}

-- | Codec for string values.
string :: Key -> TomlCodec String
string = match _String
{-# INLINE string #-}

-- | Codec for values with a 'Read' and 'Show' instance.
read :: (Show a, Read a) => Key -> TomlCodec a
read = match _Read
{-# INLINE read #-}

{- | Codec for general nullary sum data types with a 'Bounded', 'Enum', and
'Show' instance. This codec provides much better error messages than 'read' for
nullary sum types.

@since 1.1.1.0
-}
enumBounded :: (Bounded a, Enum a, Show a) => Key -> TomlCodec a
enumBounded = match _EnumBounded
{-# INLINE enumBounded #-}

-- | Codec for text values as 'ByteString'.
byteString :: Key -> TomlCodec ByteString
byteString = match _ByteString
{-# INLINE byteString #-}

-- | Codec for text values as 'BL.ByteString'.
lazyByteString :: Key -> TomlCodec BL.ByteString
lazyByteString = match _LByteString
{-# INLINE lazyByteString #-}

-- | Codec for positive integer array values as 'ByteString'.
byteStringArray :: Key -> TomlCodec ByteString
byteStringArray = match _ByteStringArray
{-# INLINE byteStringArray #-}

-- | Codec for positive integer array values as lazy 'ByteString'.
lazyByteStringArray :: Key -> TomlCodec BL.ByteString
lazyByteStringArray = match _LByteStringArray
{-# INLINE lazyByteStringArray #-}

-- | Codec for zoned time values.
zonedTime :: Key -> TomlCodec ZonedTime
zonedTime = match _ZonedTime
{-# INLINE zonedTime #-}

-- | Codec for local time values.
localTime :: Key -> TomlCodec LocalTime
localTime = match _LocalTime
{-# INLINE localTime #-}

-- | Codec for day values.
day :: Key -> TomlCodec Day
day = match _Day
{-# INLINE day #-}

-- | Codec for time of day values.
timeOfDay :: Key -> TomlCodec TimeOfDay
timeOfDay = match _TimeOfDay
{-# INLINE timeOfDay #-}

-- | Codec for list of values. Takes converter for single value and
-- returns a list of values.
arrayOf :: TomlBiMap a AnyValue -> Key -> TomlCodec [a]
arrayOf = match . _Array
{-# INLINE arrayOf #-}

-- | Codec for sets. Takes converter for single value and
-- returns a set of values.
arraySetOf :: Ord a => TomlBiMap a AnyValue -> Key -> TomlCodec (Set a)
arraySetOf = match . _Set
{-# INLINE arraySetOf #-}

-- | Codec for sets of ints. Takes converter for single value and
-- returns a set of ints.
arrayIntSet :: Key -> TomlCodec IntSet
arrayIntSet = match _IntSet
{-# INLINE arrayIntSet #-}

-- | Codec for hash sets. Takes converter for single hashable value and
-- returns a set of hashable values.
arrayHashSetOf
    :: (Hashable a, Eq a)
    => TomlBiMap a AnyValue
    -> Key
    -> TomlCodec (HashSet a)
arrayHashSetOf = match . _HashSet
{-# INLINE arrayHashSetOf #-}

-- | Codec for non- empty lists of values. Takes converter for single value and
-- returns a non-empty list of values.
arrayNonEmptyOf :: TomlBiMap a AnyValue -> Key -> TomlCodec (NonEmpty a)
arrayNonEmptyOf = match . _NonEmpty
{-# INLINE arrayNonEmptyOf #-}

{- | Prepends given key to all errors that contain key. This function is used to
give better error messages. So when error happens we know all pieces of table
key, not only the last one.
-}
handleErrorInTable :: Key -> DecodeException -> Env a
handleErrorInTable key = \case
    KeyNotFound name        -> throwError $ KeyNotFound (key <> name)
    TableNotFound name      -> throwError $ TableNotFound (key <> name)
    TypeMismatch name t1 t2 -> throwError $ TypeMismatch (key <> name) t1 t2
    e                       -> throwError e

-- | Run 'codecRead' function with given 'TOML' inside 'Control.Monad.Reader.ReaderT' context.
codecReadTOML :: TOML -> TomlCodec a -> Env a
codecReadTOML toml codec = local (const toml) (codecRead codec)

-- | Codec for tables. Use it when when you have nested objects.
table :: forall a . TomlCodec a -> Key -> TomlCodec a
table codec key = Codec input output
  where
    input :: Env a
    input = do
        mTable <- asks $ Prefix.lookup key . tomlTables
        case mTable of
            Nothing   -> throwError $ TableNotFound key
            Just toml -> codecReadTOML toml codec `catchError` handleErrorInTable key

    output :: a -> St a
    output a = do
        mTable <- gets $ Prefix.lookup key . tomlTables
        let toml = fromMaybe mempty mTable
        let newToml = execState (runMaybeT $ codecWrite codec a) toml
        a <$ modify (insertTable key newToml)

{- | 'Codec' for 'NonEmpty' list of values. Represented in TOML as array of
tables.
-}
nonEmpty :: forall a . TomlCodec a -> Key -> TomlCodec (NonEmpty a)
nonEmpty codec key = Codec input output
  where
    input :: Env (NonEmpty a)
    input = do
        mTables <- asks $ HashMap.lookup key . tomlTableArrays
        case mTables of
            Nothing    -> throwError $ TableNotFound key
            Just tomls -> forM tomls $ \toml ->
                codecReadTOML toml codec `catchError` handleErrorInTable key

    -- adds all TOML objects to the existing list if there are some
    output :: NonEmpty a -> St (NonEmpty a)
    output as = do
        let tomls = fmap (execTomlCodec codec) as
        mTables <- gets $ HashMap.lookup key . tomlTableArrays

        let newTomls = case mTables of
                Nothing       -> tomls
                Just oldTomls -> oldTomls <> tomls

        as <$ modify (insertTableArrays key newTomls)

-- | 'Codec' for list of values. Represented in TOML as array of tables.
list :: forall a . TomlCodec a -> Key -> TomlCodec [a]
list codec key = Codec
    { codecRead = (toList <$> codecRead nonEmptyCodec) `catchError` \case
        TableNotFound errKey | errKey == key -> pure []
        err -> throwError err
    , codecWrite = \case
        [] -> pure []
        l@(x:xs) -> l <$ codecWrite nonEmptyCodec (x :| xs)
    }
  where
    nonEmptyCodec :: TomlCodec (NonEmpty a)
    nonEmptyCodec = nonEmpty codec key

-- | 'Codec' for set of values. Represented in TOML as array of tables.
set :: forall a . Ord a => TomlCodec a -> Key -> TomlCodec (Set a)
set codec key = dimap S.toList S.fromList (list codec key)
{-# INLINE set #-}

-- | 'Codec' for HashSet of values. Represented in TOML as array of tables.
hashSet :: forall a . (Hashable a, Eq a) => TomlCodec a -> Key -> TomlCodec (HashSet a)
hashSet codec key = dimap HS.toList HS.fromList (list codec key)
{-# INLINE hashSet #-}
