{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Benchmark.TomlParser
       ( decode
       , parse
       , convert
       ) where

import Control.DeepSeq (NFData, rnf, rwhnf)
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)

import Benchmark.Type (FruitInside (..), HaskellType (..), SizeInside (..))

import qualified TOML


-- | Decode toml file to Haskell type.
decode :: Text -> Either String HaskellType
decode txt = case parse txt of
    Left err   -> error $ "'toml-parser' parsing failed: " <> show err
    Right toml -> maybe (Left "Some error during conversion") Right $ convert toml

-- | Wrapper over 'TOML.parseTOML'
parse :: Text -> Either TOML.TOMLError [(Text, TOML.Value)]
parse = TOML.parseTOML

-- | Convert from already parsed toml to Haskell type.
convert :: [(Text, TOML.Value)] -> Maybe HaskellType
convert toml = do
    TOML.String htTitle <- lookup "title" toml
    TOML.Double htAtom <- lookup "atom" toml
    TOML.Bool htCash <- lookup "cash" toml

    TOML.List tomlWords <- lookup "words" toml
    htWords <- mapM matchText tomlWords

    TOML.List tomlBools <- lookup "bool" toml
    htBool <- mapM matchBool tomlBools

    TOML.ZonedTimeV htToday <- lookup "today" toml

    TOML.List tomlInts <- lookup "ints" toml
    htInteger <- mapM matchInteger tomlInts

    TOML.Table fruit <- lookup "fruit" toml
    htFruit <- do
        TOML.String fiName <- lookup "name" fruit
        TOML.String fiDescription <- lookup "description" fruit
        pure FruitInside{..}

    TOML.Table size <- lookup "size" toml
    htSize <- do
        TOML.List dimensions <- lookup "dimensions" size
        arrays :: [[TOML.Value]] <- mapM matchArray dimensions
        unSize <- mapM (mapM matchDouble) arrays
        pure SizeInside{..}

    pure HaskellType{..}

matchText :: TOML.Value -> Maybe Text
matchText (TOML.String t) = Just t
matchText _               = Nothing

matchBool :: TOML.Value -> Maybe Bool
matchBool (TOML.Bool b) = Just b
matchBool _             = Nothing

matchInteger :: TOML.Value -> Maybe Integer
matchInteger (TOML.Integer i) = Just i
matchInteger _                = Nothing

matchArray :: TOML.Value -> Maybe [TOML.Value]
matchArray (TOML.List a) = Just a
matchArray _             = Nothing

matchDouble :: TOML.Value -> Maybe Double
matchDouble (TOML.Double d) = Just d
matchDouble _               = Nothing

deriving instance Generic TOML.Value
deriving instance NFData  TOML.Value

instance NFData TOML.TOMLError where
  rnf = rwhnf
