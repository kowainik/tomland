module Test.Toml.Codec.Generic
    ( genericSpec
    ) where

import Hedgehog (forAll, tripping, (===))
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, parallel)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.Codec.SmallType (SmallType, genSmallType, smallTypeCodec)
import Toml.Codec.Code (decode, encode)
import Toml.Codec.Generic (genericCodec)
import Toml.Codec.Types (TomlCodec)


genericSpec :: Spec
genericSpec = parallel $ describe "Generic codecs tests" $ do
    genericRoundtripSpec
    genericDecodeSpec

genericRoundtripSpec :: SpecWith (Arg Expectation)
genericRoundtripSpec = it "genericDecode . genericEncode ≡ id" $ hedgehog $ do
    smallType <- forAll genSmallType
    tripping smallType (encode smallTypeGenericCodec) (decode smallTypeGenericCodec)

genericDecodeSpec :: SpecWith (Arg Expectation)
genericDecodeSpec = it "genericDecode . genericEncode ≡ decode . encode" $ hedgehog $ do
    smallType <- forAll genSmallType
    let genericDecode = decode smallTypeGenericCodec . encode smallTypeGenericCodec
    let manualDecode  = decode smallTypeCodec . encode smallTypeCodec
    genericDecode smallType === manualDecode smallType

smallTypeGenericCodec :: TomlCodec SmallType
smallTypeGenericCodec = genericCodec
