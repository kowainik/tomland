{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}

-- | Contains TOML-specific combinators for converting between TOML and user data types.

module Toml.Bi.Combinators
       ( -- * Toml codecs
         bool
       , int
       , integer
       , double
       , text
       , string
       , zonedTime
       , localTime
       , day
       , timeOfDay
       , arrayOf

         -- * Combinators
       , match
       , table
       , wrapper
       , dimapNum
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

import Toml.Bi.Code (DecodeException (..), Env, St, TomlCodec)
import Toml.Bi.Monad (BiCodec, Codec (..), dimap)
import Toml.BiMap (BiMap (..), matchValueBackward, _Array, _Bool, _Double,
                   _Integer, _String, _Text, _ZonedTime, _LocalTime, _Day,
                   _TimeOfDay)
import Toml.Parser (ParseException (..))
import Toml.PrefixTree (Key)
import Toml.Type (AnyValue (..), TOML (..), TValue (..), Value (..),
                  insertKeyAnyVal, insertTable, valueType)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Toml.PrefixTree as Prefix

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

-- | Helper dimapper to turn 'integer' parser into parser for 'Int', 'Natural', 'Word', etc.
dimapNum
    :: forall n r w . (Integral n, Functor r, Functor w)
    => BiCodec r w Integer
    -> BiCodec r w n
dimapNum = dimap toInteger fromIntegral

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
int = dimapNum . integer

-- | Parser for floating values.
double :: Key -> TomlCodec Double
double = match _Double

-- | Parser for string values.
text :: Key -> TomlCodec Text
text = match _Text

-- | Codec for 'String'.
string :: Key -> TomlCodec String
string = match _String

zonedTime :: Key -> TomlCodec ZonedTime
zonedTime = match _ZonedTime

localTime :: Key -> TomlCodec LocalTime
localTime = match _LocalTime

day :: Key -> TomlCodec Day
day = match _Day

timeOfDay :: Key -> TomlCodec TimeOfDay
timeOfDay = match _TimeOfDay

-- TODO: implement using bijectionMaker
-- | Parser for array of values. Takes converter for single array element and
-- returns list of values.
arrayOf :: forall a . Typeable a => BiMap a AnyValue -> Key -> TomlCodec [a]
arrayOf bimap key = Codec input output
  where
    input :: Env [a]
    input = do
        mVal <- asks $ HashMap.lookup key . tomlPairs
        case mVal of
            Nothing -> throwError $ KeyNotFound key
            Just (AnyValue (Array arr)) -> case arr of
                []      -> pure []
                l@(x:_) -> case mapM (matchValueBackward bimap) l of
                    Nothing   -> throwError $ TypeMismatch key (typeName @a) (valueType x)
                    Just vals -> pure vals
            Just _ -> throwError $ TypeMismatch key (typeName @a) TArray

    output :: [a] -> St [a]
    output a = do
        anyVal <- MaybeT $ pure $ forward (_Array bimap) a
        a <$ modify (\(TOML vals tables) -> TOML (HashMap.insert key anyVal vals) tables)

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
