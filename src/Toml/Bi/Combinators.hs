{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
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
       , dimapNum

         -- * Toml parsers
       , bool
       , int
       , integer
       , double
       , str
       , arrayOf

         -- * Value parsers
       , Valuer (..)
       , boolV
       , integerV
       , doubleV
       , strV
       , arrV
       ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.State (State, gets, modify, runState)
import Data.Bifunctor (first)
import Data.Text (Text)

import Toml.Bi.Monad (Bi, Bijection (..), dimapBijection)
import Toml.Parser (ParseException, parse)
import Toml.PrefixTree (Key)
import Toml.Printer (prettyToml)
import Toml.Type (AnyValue (..), TOML (..), Value (..), ValueType (..), matchArray, matchBool,
                  matchDouble, matchInteger, matchText)

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

-- | Specialied 'Bi' type alias for 'Toml' monad. Keeps 'TOML' object either as
-- environment or state.
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

fromRight :: b -> Either a b -> b
fromRight b (Left _)  = b
fromRight _ (Right b) = b

-- | Unsafe version of 'decode' function if you're sure that you decoding
-- of structure is correct.
unsafeDecode :: BiToml a -> a -> Text
unsafeDecode biToml text = fromRight (error "Unsafe decode") $ decode biToml text

----------------------------------------------------------------------------
-- Generalized versions of parsers
----------------------------------------------------------------------------

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

-- | Helper dimapper to turn 'integer' parser into parser for 'Int', 'Natural', 'Word', etc.
dimapNum :: forall n r w . (Integral n, Functor r, Functor w)
         => Bi r w Integer
         -> Bi r w n
dimapNum = dimapBijection toInteger fromIntegral

----------------------------------------------------------------------------
-- Value parsers
----------------------------------------------------------------------------

-- | This data type describes how to convert value of type @a@ into and from 'Value'.
data Valuer (tag :: ValueType) a = Valuer
  { valFrom :: forall t . Value t -> Maybe a
  , valTo   :: a -> Value tag
  }

-- | 'Bool' parser for array element. Use with 'arrayOf' parser.
boolV :: Valuer 'TBool Bool
boolV = Valuer matchBool Bool

-- | 'Int' parser for array element. Use with 'arrayOf' parser.
integerV :: Valuer 'TInt Integer
integerV = Valuer matchInteger Int

-- | 'Double' parser for array element. Use with 'arrayOf' parser.
doubleV :: Valuer 'TFloat Double
doubleV = Valuer matchDouble Float

-- | 'Text' parser for array element. Use with 'arrayOf' parser.
strV :: Valuer 'TString Text
strV = Valuer matchText String

-- | Parser for array element which is an array itself. Use with 'arrayOf' parser.
arrV :: forall a t . Valuer t a -> Valuer 'TArray [a]
arrV Valuer{..} = Valuer (matchArray valFrom) (Array . map valTo)

----------------------------------------------------------------------------
-- Toml parsers
----------------------------------------------------------------------------

-- | Parser for boolean values.
bool :: Key -> BiToml Bool
bool = bijectionMaker "Boolean" matchBool Bool

-- | Parser for integer values.
integer :: Key -> BiToml Integer
integer = bijectionMaker "Int" matchInteger Int

-- | Parser for integer values.
int :: Key -> BiToml Int
int = dimapNum . integer

-- | Parser for floating values.
double :: Key -> BiToml Double
double = bijectionMaker "Double" matchDouble Float

-- | Parser for string values.
str :: Key -> BiToml Text
str = bijectionMaker "String" matchText String

-- | Parser for array of values. Takes converter for single array element and
-- returns list of values.
arrayOf :: forall a t . Valuer t a -> Key -> BiToml [a]
arrayOf valuer key = Bijection input output
  where
    input :: Env [a]
    input = do
        mVal <- asks $ HashMap.lookup key . tomlPairs
        case mVal of
            Nothing -> throwError $ KeyNotFound key
            Just (AnyValue (Array arr)) -> case arr of
                [] -> pure []
                xs -> case mapM (valFrom valuer) xs of
                    Nothing   -> throwError $ TypeMismatch "Some type of element"  -- TODO: better type
                    Just vals -> pure vals
            Just _ -> throwError $ TypeMismatch "Array of smth"

    output :: [a] -> St [a]
    output a = do
        let val = AnyValue $ Array $ map (valTo valuer) a
        mVal <- gets $ HashMap.lookup key . tomlPairs
        case mVal of
            Nothing -> a <$ modify (\(TOML vals nested) -> TOML (HashMap.insert key val vals) nested)
            Just _  -> throwError $ DuplicateKey key val
