module Test.Toml.Codec.Combinator.Common
    ( codecRoundtrip

      -- * Double helpers
    , Batman (..)
    , batmanDoubleCodec
    , batmanFloatCodec
    ) where

import Hedgehog (Gen, forAll, tripping)
import Test.Hspec (Arg, Expectation, SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)

import Toml.Codec.Code (decode, encode)
import Toml.Codec.Types (TomlCodec)
import Toml.Type.Key (Key)


import qualified Toml.Codec as Toml


codecRoundtrip
    :: forall a
    .  (Eq a, Show a)
    => String
    -> (Key -> TomlCodec a)
    -> Gen a
    -> SpecWith (Arg Expectation)
codecRoundtrip typeName mkCodec genA = it label $ hedgehog $ do
    a <- forAll genA
    let codec = mkCodec "a"
    tripping a (encode codec) (decode codec)
  where
    label :: String
    label = typeName ++ ": decode . encode ≡ id"

-- | Wrapper over 'Double' and 'Float' to be equal on @NaN@ values.
newtype Batman a = Batman
    { unBatman :: a
    } deriving stock (Show)

instance Toml.HasCodec a => Toml.HasCodec (Batman a) where
    hasCodec = Toml.diwrap . Toml.hasCodec @a

instance RealFloat a => Eq (Batman a) where
    Batman a == Batman b =
        if isNaN a
            then isNaN b
            else a == b

batmanDoubleCodec :: Key -> TomlCodec (Batman Double)
batmanDoubleCodec = Toml.diwrap . Toml.double

batmanFloatCodec :: Key -> TomlCodec (Batman Float)
batmanFloatCodec = Toml.diwrap . Toml.float
