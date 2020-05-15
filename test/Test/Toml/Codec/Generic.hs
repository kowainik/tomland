module Test.Toml.Codec.Generic
    ( genericSpec
    ) where

import Hedgehog (forAll, tripping, (===))
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.BigType (bigTypeCodec, bigTypeGenericCodec, genBigType)
import Toml.Codec.Code (decode, encode)


genericSpec :: Spec
genericSpec = describe "Generic codecs tests" $ do
    genericRoundtripSpec
    genericDecodeSpec

genericRoundtripSpec :: SpecWith (Arg Expectation)
genericRoundtripSpec = it "genericDecode . genericEncode ≡ id" $ hedgehog $ do
    bigType <- forAll genBigType
    tripping bigType (encode bigTypeGenericCodec) (decode bigTypeGenericCodec)

genericDecodeSpec :: SpecWith (Arg Expectation)
genericDecodeSpec = it "genericDecode . genericEncode ≡ decode . encode" $ hedgehog $ do
    bigType <- forAll genBigType
    let genericDecode = decode bigTypeGenericCodec . encode bigTypeGenericCodec
    let manualDecode  = decode bigTypeCodec . encode bigTypeCodec
    genericDecode bigType === manualDecode bigType
