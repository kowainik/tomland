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
       , read
       , string
       , byteString
       , lbyteString
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
       , wrapper
       , mdimap
       ) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (execState, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Coerce (Coercible, coerce)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.ByteString (ByteString)
import Data.Word (Word)
import Numeric.Natural (Natural)
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.HashSet (HashSet)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)

import Toml.Bi.Code (DecodeException (..), Env, St, TomlCodec)
import Toml.Bi.Monad (Codec (..), dimap)
import Toml.BiMap (BiMap (..), _Array, _Bool, _Double,
                   _Integer, _String, _Text, _ZonedTime, _LocalTime, _Day,
                   _TimeOfDay, _Int, _Word, _Natural, _Float, _Read,
                   _ByteString, _LByteString, _Set, _IntSet, _HashSet,
                   _NonEmpty)
import Toml.Parser (ParseException (..))
import Toml.PrefixTree (Key)
import Toml.Type (AnyValue (..), TOML (..), insertKeyAnyVal, insertTable, valueType)

import Prelude hiding (read)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Toml.PrefixTree as Prefix
import qualified Data.ByteString.Lazy as BL

----------------------------------------------------------------------------
-- Generalized versions of parsers
----------------------------------------------------------------------------

typeName :: forall a . Typeable a => Text
typeName = Text.pack $ show $ typeRep $ Proxy @a

{- | General function to create bidirectional converters for values.
-}
match :: forall a . Typeable a => BiMap a AnyValue -> Key -> TomlCodec a
match BiMap{..} key = Codec input output
  where
    input :: Env a
    input = do
        mVal <- asks $ HashMap.lookup key . tomlPairs
        case mVal of
            Nothing -> throwError $ KeyNotFound key
            Just anyVal@(AnyValue val) -> case backward anyVal of
                Just v  -> pure v
                Nothing -> throwError $ TypeMismatch key (typeName @a) (valueType val)

    output :: a -> St a
    output a = do
        anyVal <- MaybeT $ pure $ forward a
        a <$ modify (insertKeyAnyVal key anyVal)

{- | Almost same as 'dimap'. Useful when you want to have fields like this
inside your configuration:

@
data GhcVer = Ghc7103 | Ghc802 | Ghc822 | Ghc842

showGhcVer  :: GhcVer -> Text
parseGhcVer :: Text -> Maybe GhcVer
@

When you specify couple of functions of the following types:

@
show  :: a -> Text
parse :: Text -> Maybe a
@

they should satisfy property @parse . show == Just@ if you want to use your
converter for pretty-printing.
-}
mdimap :: (Monad r, Monad w, MonadError DecodeException r)
       => (c -> d)  -- ^ Convert from safe to unsafe value
       -> (a -> Maybe b)  -- ^ Parser for more type safe value
       -> Codec r w d a  -- ^ Source 'Codec' object
       -> Codec r w c b
mdimap toString toMaybe codec = Codec
  { codecRead  = (toMaybe <$> codecRead codec) >>= \case
        Nothing -> throwError $ ParseError $ ParseException "Can't parse" -- TODO
        Just b  -> pure b

  , codecWrite = \s -> do
        retS <- codecWrite codec $ toString s
        case toMaybe retS of
            Nothing -> error $ "Given pair of functions for 'mdimap' doesn't satisfy roundtrip property"
            Just b  -> pure b
  }

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

-- | Parser for string values as string.
string :: Key -> TomlCodec String
string = match _String

-- | Parser for values with a `Read` and `Show` instance.
read :: (Show a, Read a, Typeable a) => Key -> TomlCodec a
read = match _Read

-- | Parser for byte vectors values as strict bytestring.
byteString :: Key -> TomlCodec ByteString
byteString = match _ByteString

-- | Parser for byte vectors values as lazy bytestring.
lbyteString :: Key -> TomlCodec BL.ByteString
lbyteString = match _LByteString

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
arrayOf :: Typeable a => BiMap a AnyValue -> Key -> TomlCodec [a]
arrayOf = match . _Array

-- | Parser for sets. Takes converter for single value and
-- returns a set of values.
arraySetOf :: (Typeable a, Ord a) => BiMap a AnyValue -> Key -> TomlCodec (Set a)
arraySetOf = match . _Set

-- | Parser for sets of ints. Takes converter for single value and
-- returns a set of ints.
arrayIntSet :: Key -> TomlCodec IntSet
arrayIntSet = match _IntSet

-- | Parser for hash sets. Takes converter for single hashable value and
-- returns a set of hashable values.
arrayHashSetOf :: (Typeable a, Hashable a, Eq a) => BiMap a AnyValue -> Key -> TomlCodec (HashSet a)
arrayHashSetOf = match . _HashSet

-- | Parser for non- empty lists of values. Takes converter for single value and
-- returns a non-empty list of values.
arrayNonEmptyOf :: Typeable a => BiMap a AnyValue -> Key -> TomlCodec (NonEmpty a)
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

-- | Used for @newtype@ wrappers.
wrapper :: forall b a . Coercible a b => (Key -> TomlCodec a) -> Key -> TomlCodec b
wrapper bi key = dimap coerce coerce (bi key)
