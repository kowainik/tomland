{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains TOML-specific combinators for converting between TOML and user data types.

module Toml.Bi.Combinators
       ( -- * Types
         BiToml
       , Env
       , St

         -- * Exceptions
       , EncodeException
       , DecodeException

         -- * Encode/Decode
       , encode
       , decode
       , unsafeDecode

         -- * Converters
       , bijectionMaker
       , bool
       , int
       , double
       , str
       ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.State (State, gets, modify, runState)
import Data.Bifunctor (first)
import Data.Either (fromRight)
import Data.Text (Text)

import Toml.Bi.Monad (Bi, Bijection (..))
import Toml.Parser (ParseException, parse)
import Toml.PrefixTree (Key)
import Toml.Printer (prettyToml)
import Toml.Type (AnyValue (..), TOML (..), Value (..))

import qualified Data.HashMap.Strict as HashMap

-- | Type of exception for converting from 'Toml' to user custom data type.
data EncodeException
    = KeyNotFound Key  -- ^ such key is not present in 'Toml'
    | TypeMismatch Text -- ^ Expected type; TODO: add actual type
    | ParseError ParseException  -- ^ Exception during parsing
    deriving (Eq, Show)  -- TODO: manual pretty show instances

-- | Immutable environment for 'Toml' conversion.
-- This is @r@ type variable in 'Bijection' data type.
type Env = ExceptT EncodeException (Reader TOML)

-- | Write exception for convertion to 'Toml' from user custom data type.
data DecodeException
    = DuplicateKey Key AnyValue  -- ^ Key is already in table for some value
    deriving (Eq, Show)  -- TODO: manual pretty show instances

-- | Mutable context for 'Toml' conversion.
-- This is @w@ type variable in 'Bijection' data type.
type St = ExceptT DecodeException (State TOML)

-- | Specialied for 'Toml' monad.
type BiToml a = Bi Env St a

-- | Convert textual representation of toml into user data type.
encode :: BiToml a -> Text -> Either EncodeException a
encode biToml text = do
    toml <- first ParseError (parse text)
    runReader (runExceptT $ biRead biToml) toml

-- | Convert object to textual representation.
decode :: BiToml a -> a -> Either DecodeException Text
decode biToml obj = do
    -- this pair has type (TOML, Either DecodeException a)
    let (result, toml) = runState (runExceptT $ biWrite biToml obj) (TOML mempty mempty)

    -- just to trigger error if Left
    _ <- result

    pure $ prettyToml toml

-- | Unsafe version of 'decode' function if you're sure that you decoding
-- structure is correct.
unsafeDecode :: BiToml a -> a -> Text
unsafeDecode biToml text = fromRight (error "Unsafe decode") $ decode biToml text

-- | General function to create bidirectional converters for values.
bijectionMaker :: forall a t .
                 Text                              -- ^ Name of expected type
               -> (forall f . Value f -> Maybe a)  -- ^ How to convert from 'AnyValue' to @a@
               -> (a -> Value t)                   -- ^ Convert @a@ to 'Anyvale'
               -> Key                              -- ^ Key of the value
               -> BiToml a
bijectionMaker typeTag fromVal toVal key = Bijection input output
  where
    input :: Env a
    input = do
        mVal <- asks $ HashMap.lookup key . tomlPairs
        case mVal of
            Nothing -> throwError $ KeyNotFound key
            Just (AnyValue val) -> case fromVal val of
                Just v  -> pure v
                Nothing -> throwError $ TypeMismatch typeTag

    output :: a -> St a
    output a = do
        let val = AnyValue (toVal a)
        mVal <- gets $ HashMap.lookup key . tomlPairs
        case mVal of
            Nothing -> a <$ modify (\(TOML vals nested) -> TOML (HashMap.insert key val vals) nested)
            Just _  -> throwError $ DuplicateKey key val

-- | Parser for boolean values.
bool :: Key -> BiToml Bool
bool = bijectionMaker "Boolean" fromBool Bool
  where
    fromBool :: Value f -> Maybe Bool
    fromBool (Bool b) = Just b
    fromBool _        = Nothing

-- | Parser for integer values.
int :: Key -> BiToml Int
int = bijectionMaker "Int" fromInt (Int . toInteger)
  where
    fromInt :: Value f -> Maybe Int
    fromInt (Int n) = Just (fromIntegral n)
    fromInt _       = Nothing

-- | Parser for floating values.
double :: Key -> BiToml Double
double = bijectionMaker "Double" fromDouble Float
  where
    fromDouble :: Value f -> Maybe Double
    fromDouble (Float f) = Just f
    fromDouble _         = Nothing

-- | Parser for string values.
str :: Key -> BiToml Text
str = bijectionMaker "String" fromString String
  where
    fromString :: Value f -> Maybe Text
    fromString (String s) = Just s
    fromString _          = Nothing
