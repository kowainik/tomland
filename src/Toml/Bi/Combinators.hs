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
       , codecShow
       , string
       , bytestring
       , lBytestring
       , zonedTime
       , localTime
       , day
       , timeOfDay
       , arrayOf
       , setOf
       , intSet
       , hashSetOf
       , nonEmptyOf

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
import Toml.Bi.Monad (BiCodec, Codec (..), dimap)
import Toml.BiMap (BiMap (..), _Array, _Bool, _Double,
                   _Integer, _String, _Text, _ZonedTime, _LocalTime, _Day,
                   _TimeOfDay, _Int, _Word, _Natural, _Float, _Show,
                   _ByteString, _LByteString, _Set, _IntSet, _HashSet,
                   _NonEmpty)
import Toml.Parser (ParseException (..))
import Toml.PrefixTree (Key)
import Toml.Type (AnyValue (..), TOML (..), insertKeyAnyVal, insertTable, valueType)

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

natural :: Key -> TomlCodec Natural
natural = match _Natural

word :: Key -> TomlCodec Word
word = match _Word

-- | Parser for floating values.
double :: Key -> TomlCodec Double
double = match _Double

float :: Key -> TomlCodec Float
float = match _Float

-- | Parser for string values.
text :: Key -> TomlCodec Text
text = match _Text

-- | Codec for 'String'.
string :: Key -> TomlCodec String
string = match _String

codecShow :: (Show a, Read a, Typeable a) => Key -> TomlCodec a
codecShow = match _Show

bytestring :: Key -> TomlCodec ByteString
bytestring = match _ByteString

lBytestring :: Key -> TomlCodec BL.ByteString
lBytestring = match _LByteString

zonedTime :: Key -> TomlCodec ZonedTime
zonedTime = match _ZonedTime

localTime :: Key -> TomlCodec LocalTime
localTime = match _LocalTime

day :: Key -> TomlCodec Day
day = match _Day

timeOfDay :: Key -> TomlCodec TimeOfDay
timeOfDay = match _TimeOfDay

-- | Parser for list of values. Takes converter for single array element and
-- returns list of values.
arrayOf :: Typeable a => BiMap a AnyValue -> Key -> TomlCodec [a]
arrayOf bimap = match (_Array bimap)

setOf :: (Typeable a, Ord a) => BiMap a AnyValue -> Key -> TomlCodec (Set a)
setOf bimap = match (_Set bimap)

intSet :: Key -> TomlCodec IntSet
intSet = match _IntSet

hashSetOf :: (Typeable a, Hashable a, Eq a) => BiMap a AnyValue -> Key -> TomlCodec (HashSet a)
hashSetOf bimap = match (_HashSet bimap)

nonEmptyOf :: Typeable a => BiMap a AnyValue -> Key -> TomlCodec (NonEmpty a)
nonEmptyOf bimap = match (_NonEmpty bimap)

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
