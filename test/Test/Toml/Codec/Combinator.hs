module Test.Toml.Codec.Combinator
    ( combinatorSpec
    ) where

import Hedgehog (forAll, tripping)
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

-- import Test.Toml.BigType (bigTypeCodec, genBigType)
import Test.Toml.Codec.Combinator.Primitive (primitiveSpec)
import Toml.Codec.Code (decode, encode)


combinatorSpec :: Spec
combinatorSpec = describe "Combinator spec"
    primitiveSpec

--     encodeDecodeSpec
--
-- encodeDecodeSpec :: SpecWith (Arg Expectation)
-- encodeDecodeSpec = it "decode . encode ≡ id" $ hedgehog $ do
--     bigType <- forAll genBigType
--     tripping bigType (encode bigTypeCodec) (decode bigTypeCodec)
