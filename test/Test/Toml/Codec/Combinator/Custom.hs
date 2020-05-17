module Test.Toml.Codec.Combinator.Custom
    ( customSpec
    ) where

import Data.Bifunctor (first)
import Test.Hspec (Spec, describe)
import Text.Read (readEither)

import Test.Toml.Codec.Combinator.Common (codecRoundtrip)

import qualified Data.Text as Text
import qualified Hedgehog.Gen as Gen
import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec.BiMap.Conversion as Toml
import qualified Toml.Codec.Combinator.Custom as Toml


customSpec :: Spec
customSpec = describe "Combinator.Custom: Roundtrip tests" $ do
    codecRoundtrip "Read (Int)      " (Toml.read @Int) Gen.genInt
    codecRoundtrip "Enum (Ordering) " (Toml.enumBounded @Ordering) Gen.enumBounded
    codecRoundtrip "Validate (Even) "
        (Toml.validateIf even Toml._Int)
        ((* 2) <$> Gen.genInt)
    codecRoundtrip "TextBy          "
        (Toml.textBy (Text.pack . show) (first Text.pack . readEither . Text.unpack))
        Gen.genInt
