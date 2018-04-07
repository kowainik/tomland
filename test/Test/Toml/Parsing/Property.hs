{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Toml.Parsing.Property
       ( propertyTests
       ) where

import Hedgehog (Property, forAll, property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Toml.Parser (parse)
import Toml.Printer (prettyToml)

import Test.Toml.Gen (genToml)

propertyTests :: [TestTree]
propertyTests = [parsePrintTest]

parsePrintTest :: TestTree
parsePrintTest = testProperty "parse . prettyPrint == id" prop_parsePrint

prop_parsePrint :: Property
prop_parsePrint =  property $ do
    toml <- forAll genToml
    parse (prettyToml toml) === Right toml
