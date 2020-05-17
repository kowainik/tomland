module Test.Toml.Codec.SmallType
    ( smallTypeSpec

      -- * Internals
    , SmallType
    , smallTypeCodec
    , genSmallType
    ) where

import Data.Monoid (Any (..))
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Hedgehog (Gen)
import Test.Hspec (Spec, describe)

import Test.Toml.Codec.Combinator.Common (codecRoundtrip)
import Toml.Codec (TomlCodec, (.=))

import qualified Hedgehog.Gen as Gen
import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec as Toml


smallTypeSpec :: Spec
smallTypeSpec = describe "SmallType: tests for custom data type" $
    codecRoundtrip "SmallType" (Toml.table smallTypeCodec) genSmallType

data SmallType = SmallType
    { smallTypeInt       :: !Int
    , smallTypeText      :: !Text
    , smallTypeAny       :: !Any
    , smallTypeDay       :: !Day
    , smallTypeMaybeWord :: !(Maybe Word)
    , smallTypeListInt   :: ![Int]
    } deriving stock (Eq, Show, Generic)

smallTypeCodec :: TomlCodec SmallType
smallTypeCodec = SmallType
    <$> Toml.int "int" .= smallTypeInt
    <*> Toml.text "text" .= smallTypeText
    <*> Toml.any "any" .= smallTypeAny
    <*> Toml.day "day" .= smallTypeDay
    <*> Toml.dioptional (Toml.word "maybe.word") .= smallTypeMaybeWord
    <*> Toml.arrayOf Toml._Int "list.int" .= smallTypeListInt

genSmallType :: Gen SmallType
genSmallType = do
    smallTypeInt       <- Gen.genInt
    smallTypeText      <- Gen.genText
    smallTypeAny       <- Any <$> Gen.genBool
    smallTypeDay       <- Gen.genDay
    smallTypeMaybeWord <- Gen.maybe Gen.genWord
    smallTypeListInt   <- Gen.genSmallList Gen.genInt
    pure SmallType{..}
