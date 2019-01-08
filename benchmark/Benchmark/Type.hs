{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Benchmark.Type
       ( HaskellType (..)
       , FruitInside (..)
       , SizeInside (..)
       ) where

import Control.DeepSeq (NFData)
import Data.Aeson.Types (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import Data.Time (ZonedTime)
import GHC.Generics (Generic)


-- | Haskell type to convert to.
data HaskellType = HaskellType
    { htTitle   :: Text
    , htAtom    :: Double
    , htCash    :: Bool
    , htWords   :: [Text]
    , htBool    :: [Bool]
    , htToday   :: ZonedTime
    , htInteger :: [Integer]
    , htFruit   :: FruitInside
    , htSize    :: SizeInside
    } deriving (Show, NFData, Generic)

instance FromJSON HaskellType where
    parseJSON = withObject "HaskellType" $ \o -> HaskellType
        <$> o .: "title"
        <*> o .: "atom"
        <*> o .: "cash"
        <*> o .: "words"
        <*> o .: "bool"
        <*> o .: "today"
        <*> o .: "ints"
        <*> o .: "fruit"
        <*> o .: "size"

data FruitInside = FruitInside
    { fiName        :: Text
    , fiDescription :: Text
    } deriving (Show, NFData, Generic)

instance FromJSON FruitInside where
    parseJSON = withObject "FruitInside" $ \o -> FruitInside
        <$> o .: "name"
        <*> o .: "description"

newtype SizeInside = SizeInside
    { unSize :: [[Double]]
    } deriving (Show, NFData, Generic)

instance FromJSON SizeInside where
    parseJSON = withObject "SizeInside" $ \o -> SizeInside <$> o .: "dimensions"
