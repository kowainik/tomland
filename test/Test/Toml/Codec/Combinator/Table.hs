module Test.Toml.Codec.Combinator.Table
    ( tableSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Toml.Codec.Combinator.Common (codecRoundtrip)

import qualified Test.Toml.Gen as Gen
import qualified Toml.Codec.Combinator.Primitive as Toml
import qualified Toml.Codec.Combinator.Table as Toml


tableSpec :: Spec
tableSpec = describe "Combinator.Table: Roundtrip tests" $
    codecRoundtrip "Table (Int)" (Toml.table $ Toml.int "a") Gen.genInt
