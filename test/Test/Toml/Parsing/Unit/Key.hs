{-# LANGUAGE PatternSynonyms #-}

module Test.Toml.Parsing.Unit.Key
    ( keySpecs
    ) where

import Test.Hspec (Spec, context, describe, it, xit)

import Toml.Type.Key (pattern (:||))

import Test.Toml.Parsing.Unit.Common (dquote, parseKey, squote)


keySpecs :: Spec
keySpecs = describe "keyP" $ do
    context "when the key is a bare key" $ do
        it "can parse keys which contain ASCII letters, digits, underscores, and dashes" $ do
            parseKey "key"       "key"
            parseKey "bare_key1" "bare_key1"
            parseKey "bare-key2" "bare-key2"
        it "can parse keys which contain only digits" $
            parseKey "1234" "1234"
    context "when the key is a quoted key" $ do
        it "can parse keys that follow the exact same rules as basic strings" $ do
            parseKey (dquote "127.0.0.1") ("\"127.0.0.1\"" :|| [])
            parseKey (dquote "character encoding") "\"character encoding\""
            parseKey (dquote "ʎǝʞ") "\"ʎǝʞ\""
        it "can parse keys that follow the exact same rules as literal strings" $ do
            parseKey (squote "key2") "'key2'"
            parseKey (squote "quoted \"value\"") "'quoted \"value\"'"
    context "when the key is a dotted key" $ do
        it "can parse a sequence of bare or quoted keys joined with a dot" $ do
            parseKey "name"           "name"
            parseKey "physical.color" "physical.color"
            parseKey "physical.shape" "physical.shape"
            parseKey "site.\"google.com\"" ("site" :|| ["\"google.com\""])
        xit "ignores whitespaces around dot-separated parts" $
            parseKey "a . b . c. d" ("a" :|| ["b", "c", "d"])
