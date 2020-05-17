module Test.Toml.Codec.Combinator.Map
    ( mapSpec
    ) where

import Test.Hspec (Spec, describe, parallel)

import Test.Toml.Codec.Combinator.Common (codecRoundtrip)
import Toml.Type.Printer (prettyKey)

import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec.BiMap.Conversion as Toml
import qualified Toml.Codec.Combinator.Map as Toml
import qualified Toml.Codec.Combinator.Primitive as Toml


mapSpec :: Spec
mapSpec = parallel $ describe "Combinator.Map: Roundtrip tests" $ do
    codecRoundtrip "Map Int Text (map)     "
        (Toml.map (Toml.int "key") (Toml.text "val"))
        (Gen.genMap Gen.genInt Gen.genText)
    codecRoundtrip "Map Text Int (tableMap)"
        (Toml.tableMap Toml._KeyText Toml.int)
        (Gen.genMap (prettyKey <$> Gen.genKey) Gen.genInt)
