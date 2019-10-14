{-# LANGUAGE FlexibleContexts #-}

module Test.Toml.Parsing.Unit.Key where

import Test.Tasty.Hspec (Spec, context, describe, it)

import Test.Toml.Parsing.Unit.Common

keySpecs :: Spec
keySpecs = describe "keyP" $ do
    context "when the key is a bare key" $ do
        it "can parse keys which contain ASCII letters, digits, underscores, and dashes" $ do
            parseKey "key"       (makeKey ["key"])
            parseKey "bare_key1" (makeKey ["bare_key1"])
            parseKey "bare-key2" (makeKey ["bare-key2"])
        it "can parse keys which contain only digits" $
            parseKey "1234" (makeKey ["1234"])
    context "when the key is a quoted key" $ do
        it "can parse keys that follow the exact same rules as basic strings" $ do
            parseKey (dquote "127.0.0.1") (makeKey [dquote "127.0.0.1"])
            parseKey (dquote "character encoding") (makeKey [dquote "character encoding"])
            parseKey (dquote "ʎǝʞ") (makeKey [dquote "ʎǝʞ"])
        it "can parse keys that follow the exact same rules as literal strings" $ do
            parseKey (squote "key2") (makeKey [squote "key2"])
            parseKey (squote "quoted \"value\"") (makeKey [squote "quoted \"value\""])
    context "when the key is a dotted key" $ do
        it "can parse a sequence of bare or quoted keys joined with a dot" $ do
            parseKey "name"           (makeKey ["name"])
            parseKey "physical.color" (makeKey ["physical", "color"])
            parseKey "physical.shape" (makeKey ["physical", "shape"])
            parseKey "site.\"google.com\"" (makeKey ["site", dquote "google.com"])
        -- it "ignores whitespaces around dot-separated parts" $ do
        --     parseKey "a . b . c. d" (makeKey ["a", "b", "c", "d"])
