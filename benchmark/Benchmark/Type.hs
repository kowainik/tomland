module Benchmark.Type
       ( HaskellType(..)
       , codec
       ) where

import Data.Text (Text)
import Data.Time (ZonedTime)
import Data.Aeson.Types (FromJSON, withObject, parseJSON, (.:))

import Toml (TomlCodec, (.=))

import qualified Toml


-- | Haskell type to convert to.
data HaskellType = HaskellType
    { htTitle  :: Text
    , htAtom   :: Double
    , htCash   :: Bool
    , htWords  :: [Text]
    , htBool   :: [Bool]
    , htToday  :: ZonedTime
    , htFruit  :: FruitInside
    , htSize   :: SizeInside
    }
    deriving Show

instance FromJSON HaskellType where
    parseJSON = withObject "HaskellType" $ \o -> HaskellType
        <$> o .: "title"
        <*> o .: "atom"
        <*> o .: "cash"
        <*> o .: "words"
        <*> o .: "bool"
        <*> o .: "today"
        <*> o .: "fruit"
        <*> o .: "size"

data FruitInside = FruitInside
    { fiName        :: Text
    , fiDescription :: Text
    }
    deriving Show

instance FromJSON FruitInside where
    parseJSON = withObject "FruitInside" $ \o -> FruitInside
        <$> o .: "name"
        <*> o .: "description"

insideF :: TomlCodec FruitInside
insideF = FruitInside
    <$> Toml.text "name" .= fiName
    <*> Toml.text "description" .= fiDescription

newtype SizeInside = SizeInside
    { unSize :: [[Double]]
    }
    deriving Show

instance FromJSON SizeInside where
    parseJSON = withObject "SizeInside" $ \o -> SizeInside <$> o .: "dimensions"

insideS :: TomlCodec SizeInside
insideS = Toml.dimap unSize SizeInside $ Toml.arrayOf (Toml._Array Toml._Double) "dimensions"

-- | Codec to use in tomland decode and convert functions.
codec :: TomlCodec HaskellType
codec = HaskellType
    <$> Toml.text "title" .= htTitle
    <*> Toml.double "atom" .= htAtom
    <*> Toml.bool "cash" .= htCash
    <*> Toml.arrayOf Toml._Text "words" .= htWords
    <*> Toml.arrayOf Toml._Bool "bool" .= htBool
    <*> Toml.zonedTime "today" .= htToday
    <*> Toml.table insideF "fruit" .= htFruit
    <*> Toml.table insideS "size" .= htSize