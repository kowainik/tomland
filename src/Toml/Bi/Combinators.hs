{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains TOML-specific combinators for converting between TOML and user data types.

module Toml.Bi.Combinators
       ( -- * Converters
         bijectionMaker
       , dimapNum
       , mdimap

         -- * Toml parsers
       , bool
       , int
       , integer
       , double
       , str
       , arrayOf
       , maybeP
       , table

         -- * Value parsers
       , Valuer (..)
       , boolV
       , integerV
       , doubleV
       , strV
       , arrV
       ) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (execState, gets, modify)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)

import Toml.Bi.Code (BiToml, DecodeException (..), Env, St)
import Toml.Bi.Monad (Bi, Bijection (..), dimap)
import Toml.Parser (ParseException (..))
import Toml.PrefixTree (Key)
import Toml.Type (AnyValue (..), TOML (..), Value (..), ValueType (..), matchArray, matchBool,
                  matchDouble, matchInteger, matchText, valueType)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Toml.PrefixTree as Prefix

----------------------------------------------------------------------------
-- Generalized versions of parsers
----------------------------------------------------------------------------

typeName :: forall a . Typeable a => Text
typeName = Text.pack $ show $ typeRep $ Proxy @a

-- | General function to create bidirectional converters for values.
bijectionMaker :: forall a t . Typeable a
               => (forall f . Value f -> Maybe a)  -- ^ How to convert from 'AnyValue' to @a@
               -> (a -> Value t)                   -- ^ Convert @a@ to 'Anyvale'
               -> Key                              -- ^ Key of the value
               -> BiToml a
bijectionMaker fromVal toVal key = Bijection input output
  where
    input :: Env a
    input = do
        mVal <- asks $ HashMap.lookup key . tomlPairs
        case mVal of
            Nothing -> throwError $ KeyNotFound key
            Just (AnyValue val) -> case fromVal val of
                Just v  -> pure v
                Nothing -> throwError $ TypeMismatch key (typeName @a) (valueType val)

    output :: a -> St a
    output a = do
        let val = AnyValue (toVal a)
        a <$ modify (\(TOML vals nested) -> TOML (HashMap.insert key val vals) nested)

-- | Helper dimapper to turn 'integer' parser into parser for 'Int', 'Natural', 'Word', etc.
dimapNum :: forall n r w . (Integral n, Functor r, Functor w)
         => Bi r w Integer
         -> Bi r w n
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
       -> Bijection r w d a  -- ^ Source 'Bijection' object
       -> Bijection r w c b
mdimap toString toMaybe bi = Bijection
  { biRead  = (toMaybe <$> biRead bi) >>= \case
        Nothing -> throwError $ ParseError $ ParseException "Can't parse" -- TODO
        Just b  -> pure b

  , biWrite = \s -> do
        retS <- biWrite bi $ toString s
        case toMaybe retS of
            Nothing -> error $ "Given pair of functions for 'mdimap' doesn't satisfy roundtrip property"
            Just b  -> pure b
  }

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
bool = bijectionMaker matchBool Bool

-- | Parser for integer values.
integer :: Key -> BiToml Integer
integer = bijectionMaker matchInteger Int

-- | Parser for integer values.
int :: Key -> BiToml Int
int = dimapNum . integer

-- | Parser for floating values.
double :: Key -> BiToml Double
double = bijectionMaker matchDouble Float

-- | Parser for string values.
str :: Key -> BiToml Text
str = bijectionMaker matchText String

-- TODO: implement using bijectionMaker
-- | Parser for array of values. Takes converter for single array element and
-- returns list of values.
arrayOf :: forall a t . Typeable a => Valuer t a -> Key -> BiToml [a]
arrayOf valuer key = Bijection input output
  where
    input :: Env [a]
    input = do
        mVal <- asks $ HashMap.lookup key . tomlPairs
        case mVal of
            Nothing -> throwError $ KeyNotFound key
            Just (AnyValue (Array arr)) -> case arr of
                []   -> pure []
                x:xs -> case mapM (valFrom valuer) (x:xs) of
                    Nothing   -> throwError $ TypeMismatch key (typeName @a) (valueType x)  -- TODO: different error for array element
                    Just vals -> pure vals
            Just _ -> throwError $ TypeMismatch key (typeName @a) TArray

    output :: [a] -> St [a]
    output a = do
        let val = AnyValue $ Array $ map (valTo valuer) a
        a <$ modify (\(TOML vals tables) -> TOML (HashMap.insert key val vals) tables)

-- TODO: maybe conflicts from maybe in Prelude, maybe we should add C or P suffix or something else?...
-- | Bidirectional converter for @Maybe smth@ values.
maybeP :: forall a . (Key -> BiToml a) -> Key -> BiToml (Maybe a)
maybeP converter key = let bi = converter key in Bijection
    { biRead  = (Just <$> biRead bi) `catchError` handleNotFound
    , biWrite = \case
        Nothing -> pure Nothing
        Just v  -> biWrite bi v >> pure (Just v)
    }
  where
    handleNotFound :: DecodeException -> Env (Maybe a)
    handleNotFound e
        | e `elem` [KeyNotFound key, TableNotFound key] = pure Nothing
        | otherwise = throwError e

-- | Parser for tables. Use it when when you have nested objects.
table :: forall a . BiToml a -> Key -> BiToml a
table bi key = Bijection input output
  where
    input :: Env a
    input = do
        mTable <- asks $ Prefix.lookup key . tomlTables
        case mTable of
            Nothing   -> throwError $ TableNotFound key
            Just toml -> local (const toml) (biRead bi) `catchError` handleTableName

    output :: a -> St a
    output a = do
        mTable <- gets $ Prefix.lookup key . tomlTables
        let toml = fromMaybe (TOML mempty mempty) mTable
        let newToml = execState (biWrite bi a) toml
        a <$ modify (\(TOML vals tables) -> TOML vals (Prefix.insert key newToml tables))

    handleTableName :: DecodeException -> Env a
    handleTableName (KeyNotFound name)        = throwError $ KeyNotFound (key <> name)
    handleTableName (TableNotFound name)      = throwError $ TableNotFound (key <> name)
    handleTableName (TypeMismatch name t1 t2) = throwError $ TypeMismatch (key <> name) t1 t2
    handleTableName e                         = throwError e
