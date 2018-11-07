module Benchmark.Tomland
       ( convert
       , decode
       , parse
       ) where

import Data.Text (Text)

import Benchmark.Type (FruitInside (..), HaskellType (..), SizeInside (..))
import Toml (DecodeException, TOML, TomlCodec, parse, (.=))

import qualified Toml


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

insideF :: TomlCodec FruitInside
insideF = FruitInside
    <$> Toml.text "name" .= fiName
    <*> Toml.text "description" .= fiDescription

insideS :: TomlCodec SizeInside
insideS = Toml.dimap unSize SizeInside $
    Toml.arrayOf (Toml._Array Toml._Double) "dimensions"

convert :: TOML -> Either DecodeException HaskellType
convert = Toml.runTomlCodec codec

decode :: Text -> Either DecodeException HaskellType
decode = Toml.decode codec
