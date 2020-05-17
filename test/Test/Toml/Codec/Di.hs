module Test.Toml.Codec.Di
    ( diSpec
    ) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Hedgehog (Gen)
import Test.Hspec (Spec, describe)

import Test.Toml.Codec.Combinator.Common (codecRoundtrip)
import Toml.Codec.Di (dimatch)
import Toml.Codec.Types (TomlCodec)
import Toml.Type.Key (Key)

import qualified Hedgehog.Gen as Gen
import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec as Toml


diSpec :: Spec
diSpec = describe "Codec.Di functions tests" $
    describe "dimatch" $
        codecRoundtrip "SumType" sumTypeExampleCodec genSumTypeExample

data SumType
    = One Bool
    | Two Int Text
    | Three [Int]
    deriving stock (Eq, Show)

matchOne :: SumType -> Maybe Bool
matchOne = \case
    One b -> Just b
    _ -> Nothing

matchTwo :: SumType -> Maybe (Int, Text)
matchTwo = \case
    Two i t -> Just (i, t)
    _ -> Nothing

matchThree :: SumType -> Maybe [Int]
matchThree = \case
    Three l -> Just l
    _ -> Nothing

sumTypeExampleCodec :: Key -> TomlCodec SumType
sumTypeExampleCodec _ =
        dimatch matchOne One (Toml.bool "one")
    <|> dimatch matchTwo (uncurry Two) (Toml.pair (Toml.int "two.a") (Toml.text "two.b"))
    <|> dimatch matchThree Three (Toml.arrayOf Toml._Int "three")

genSumTypeExample :: Gen SumType
genSumTypeExample = Gen.choice
    [ One <$> Gen.genBool
    , Two <$> Gen.genInt <*> Gen.genText
    , Three <$> Gen.genSmallList Gen.genInt
    ]
