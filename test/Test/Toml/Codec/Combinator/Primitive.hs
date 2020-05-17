module Test.Toml.Codec.Combinator.Primitive
    ( primitiveSpec
    ) where

import Test.Hspec (Spec, describe, parallel)

import Test.Toml.Codec.Combinator.Common (Batman (..), batmanDoubleCodec, batmanFloatCodec,
                                          codecRoundtrip)

import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec.Combinator.Primitive as Toml


primitiveSpec :: Spec
primitiveSpec = parallel $ describe "Combinator.Primitive: Roundtrip tests" $ do
    codecRoundtrip "Bool             " Toml.bool                Gen.genBool
    codecRoundtrip "Integer          " Toml.integer             Gen.genInteger
    codecRoundtrip "Int              " Toml.int                 Gen.genInt
    codecRoundtrip "Natural          " Toml.natural             Gen.genNatural
    codecRoundtrip "Word             " Toml.word                Gen.genWord
    codecRoundtrip "Word8            " Toml.word8               Gen.genWord8
    codecRoundtrip "Double           " batmanDoubleCodec        (Batman <$> Gen.genDouble)
    codecRoundtrip "Float            " batmanFloatCodec         (Batman <$> Gen.genFloat)
    codecRoundtrip "String           " Toml.string              Gen.genString
    codecRoundtrip "Text             " Toml.text                Gen.genText
    codecRoundtrip "LText            " Toml.lazyText            Gen.genLText
    codecRoundtrip "ByteString       " Toml.byteString          Gen.genByteString
    codecRoundtrip "LByteString      " Toml.lazyByteString      Gen.genLByteString
    codecRoundtrip "ByteString Array " Toml.byteStringArray     Gen.genByteString
    codecRoundtrip "LByteString Array" Toml.lazyByteStringArray Gen.genLByteString
