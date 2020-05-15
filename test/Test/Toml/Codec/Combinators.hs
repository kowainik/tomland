module Test.Toml.Codec.Combinators
    ( combinatorsSpec
    ) where

import Hedgehog (forAll, tripping)
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.BigType (bigTypeCodec, genBigType)
import Toml.Codec.Code (decode, encode)


combinatorsSpec :: Spec
combinatorsSpec = describe "Combinators spec" $
    encodeDecodeSpec

encodeDecodeSpec :: SpecWith (Arg Expectation)
encodeDecodeSpec = it "decode . encode ≡ id" $ hedgehog $ do
    bigType <- forAll genBigType
    tripping bigType (encode bigTypeCodec) (decode bigTypeCodec)
