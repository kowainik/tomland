{-# LANGUAGE PatternSynonyms #-}

module Test.Toml.Parser.Key
    ( keySpecs
    ) where

import Test.Hspec (Spec, context, describe, it)

import Test.Toml.Parser.Common (dquote, failOn, parseKey, squote, squote3, tomlFailOn)
import Toml.Parser.Key (keyP)
import Toml.Type.Key (pattern (:||))


keySpecs :: Spec
keySpecs = describe "keyP" $ do
    context "when the key is a bare key" $ do
        it "can parse keys which contain ASCII letters, digits, underscores, and dashes" $ do
            parseKey "key"       "key"
            parseKey "bare_key1" "bare_key1"
            parseKey "bare-key2" "bare-key2"
        it "can parse keys which contain only digits" $
            parseKey "1234" "1234"
        it "fails on non-ASCII characters" $ do
            failOn keyP "\956"
            failOn keyP "\12288foo"
            tomlFailOn "key\12290 = 1"
    context "when the key is a quoted key" $ do
        it "can parse keys that follow the exact same rules as basic strings" $ do
            parseKey (dquote "127.0.0.1") ("127.0.0.1" :|| [])
            parseKey (dquote "character encoding") ("character encoding" :|| [])
            parseKey (dquote "ʎǝʞ") ("ʎǝʞ" :|| [])
            parseKey (dquote "esc\\u0061ped\\n") ("escaped\n" :|| [])
            parseKey (dquote "") ("" :|| [])
        it "can parse keys that follow the exact same rules as literal strings" $ do
            parseKey (squote "key2") ("key2" :|| [])
            parseKey (squote "quoted \"value\"") ("quoted \"value\"" :|| [])
        it "treats bare and quoted spellings of a key as the same key" $ do
            parseKey (dquote "key") "key"
            parseKey (squote "key") "key"
            parseKey "a.\"b\".'c'" ("a" :|| ["b", "c"])
        it "fails on multi-line strings used as keys" $ do
            tomlFailOn "\"\"\"key\"\"\" = 1"
            tomlFailOn (squote3 "key" <> " = 1")
    context "when the key is a dotted key" $ do
        it "can parse a sequence of bare or quoted keys joined with a dot" $ do
            parseKey "name"           "name"
            parseKey "physical.color" "physical.color"
            parseKey "physical.shape" "physical.shape"
            parseKey "site.\"google.com\"" ("site" :|| ["google.com"])
        it "ignores whitespaces around dot-separated parts" $ do
            parseKey "a . b . c. d" ("a" :|| ["b", "c", "d"])
            parseKey "fruit. color" ("fruit" :|| ["color"])
            parseKey "a\t.\t\"b\" . 'c'" ("a" :|| ["b", "c"])
    context "when the key is symbols only" $ do
        it "parses Haskell comments" $
            parseKey "--" "--"
        it "parses Haskell comments with dots" $
            parseKey "--.--" $ "--" :|| [ "--" ]
        it "parses underscores" $ do
            parseKey "_"   "_"
            parseKey "__"  "__"
            parseKey "___" "___"
        it "parses quotes" $
            parseKey "\".\"" $ "." :|| []
