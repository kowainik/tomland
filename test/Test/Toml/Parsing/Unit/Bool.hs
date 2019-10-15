{-# LANGUAGE FlexibleContexts #-}

module Test.Toml.Parsing.Unit.Bool where

import Test.Tasty.Hspec (Spec, describe, it)

import Test.Toml.Parsing.Unit.Common (boolFailOn, parseBool)

boolSpecs :: Spec
boolSpecs = describe "boolP" $ do
    it "can parse `true` and `false`" $ do
        parseBool "true" True
        parseBool "false" False
        parseBool "true        " True
    it "fails if `true` or `false` are not all lowercase" $ do
        boolFailOn "True"
        boolFailOn "False"
        boolFailOn "TRUE"
        boolFailOn "FALSE"
        boolFailOn "tRuE"
        boolFailOn "fAlSE"
