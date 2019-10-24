{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

module Test.Toml.Parsing.Unit.Toml where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup ((<>))
import Data.Text (Text)

import Test.Tasty.Hspec (Spec, describe, it)

import Toml.Edsl (empty, mkToml, table, tableArray, (=:))
import Toml.PrefixTree (pattern (:||))
import Toml.Type (TOML (..), Value (..))

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Test.Toml.Parsing.Unit.Common (day2, parseToml, tomlFailOn)

tomlSpecs :: Spec
tomlSpecs = do
    describe "Key/values" $ do
        it "can parse key/value pairs" $ do
            parseToml "x='abcdef'" $ mkToml ("x" =: "abcdef")
            parseToml "x= 1"  $ mkToml ("x" =: 1)
            parseToml "x =5.2" $ mkToml ("x" =: Double 5.2)
            parseToml "x = true" $ mkToml ("x" =: Bool True)
            parseToml "x= [1, 2, 3]" $ mkToml ("x" =: Array [1, 2, 3])
            parseToml "x =1920-12-10" $ mkToml ("x" =: Day day2)
        it "ignores white spaces around key names and values" $ do
            let toml = mkToml ("x" =: 1)
            parseToml "x=1    "   toml
            parseToml "x=    1"   toml
            parseToml "x    =1"   toml
            parseToml "x\t= 1 "   toml
            parseToml "\"x\" = 1" $ mkToml ("\"x\"" =: 1)
        --xit "fails if the key, equals sign, and value are not on the same line" $ do
        --  keyValFailOn "x\n=\n1"
        --  keyValFailOn "x=\n1"
        --  keyValFailOn "\"x\"\n=\n1"
        it "works if the value is broken over multiple lines" $
            parseToml "x=[1, \n2\n]" $ mkToml ("x" =: Array [1, 2])
        it "fails if the value is not specified" $
            tomlFailOn "x="

    describe "tables" $ do
        it "can parse a TOML table" $ do
            let t  = mkToml $
                        table "table" $ do
                            "key1" =: "some string"
                            "key2" =: 123

            parseToml "[table] \n key1 = \"some string\"\nkey2 = 123" t
        it "can parse an empty TOML table" $
            parseToml "[table]" $ mkToml (table "table" empty)
        it "can parse a table with subarrays" $ do
            let t = mkToml $
                        table "table" $
                            tableArray "array" ("key1" =: "some string" :| ["key2" =: 123])

            parseToml "[table] \n [[table.array]] \nkey1 = \"some string\"\n \
                                  \[[table.array]] \nkey2 = 123" t
        it "can parse a TOML inline table" $
            parseToml "table={key1 = \"some string\", key2 = 123}" $
                mkToml $
                    table "table" $ do
                        "key1" =: "some string"
                        "key2" =: 123
        it "can parse an empty inline TOML table" $
            parseToml "table = {}" $ mkToml (table "table" empty)
        it "can parse a table followed by an inline table" $
            parseToml "[table1] \n  key1 = \"some string\" \n table2 = {key2 = 123}" $
                mkToml $
                    table "table1" $ do
                        "key1" =: "some string"
                        table "table2" $ "key2" =: 123
        it "can parse an empty table followed by an inline table" $
            parseToml "[table1] \n table2 = {key2 = 123}" $
                mkToml $
                    table "table1" $
                        table "table2" $
                            "key2" =: 123
        it "allows the name of the table to be any valid TOML key" $ do
            parseToml "dog.\"tater.man\"={}" $ mkToml $ table ("dog" :|| ["\"tater.man\""]) empty
            parseToml "j.\"ʞ\".'l'={}" $ mkToml $ table "j.\"ʞ\".'l'" empty

    describe "array of tables" $ do
        it "can parse an empty array" $
            parseToml "[[array]]" $ mkToml $ tableArray "array" (empty :| [])
        it "can parse an array of key/values" $ do
            let array = mkToml $
                        tableArray "array" $
                            "key1" =: "some string" :|
                            ["key2" =: 123]

            parseToml "[[array]]\n key1 = \"some string\"\n \
                       \[[array]]\n key2 = 123" array
        it "can parse an array of tables" $ do
            let table1 = table "table1" ("key1" =: "some string")
                table2 = table "table2" ("key2" =: 123)
                array = mkToml $ tableArray "array" $ table1 :| [table2]

            parseToml "[[array]]\n[array.table1] \n key1 = \"some string\"\n \
                       \[[array]]\n[array.table2] \n key2 = 123" array
        it "can parse an array of array" $ do
            let arr = tableArray "subarray" ("key1" =: "some string" :| ["key2" =: 123])
                array = mkToml $ tableArray "array" (arr :| [])

            parseToml "[[array]] \n [[array.subarray]] \nkey1 = \"some string\"\n \
                       \[[array.subarray]] \nkey2 = 123" array
        it "can parse an array of arrays" $ do
            let
                arr1 = tableArray "table-1" ("key1" =: Text "some string" :| [])
                arr2 = tableArray "table-2" ("key2" =: Integer 123 :| [])
                array = mkToml $ tableArray "array" $ (arr1 >> arr2) :| []

            parseToml "[[array]]\n [[array.table-1]] \nkey1 = \"some string\"\n \
                                     \[[array.table-2]] \nkey2 = 123" array
        it "can parse very large arrays" $ do
            let array = mkToml $ tableArray "array" $ NE.fromList $ replicate 1000 empty
            parseToml (mconcat $ replicate 1000 "[[array]]\n") array
        it "can parse an inline array of tables" $ do
            let array = mkToml $ tableArray "table" $ NE.fromList ["key1" =: "some string", "key2" =: 123]
            parseToml "table = [{key1 = \"some string\"}, {key2 = 123}]" array

    describe "TOML" $ do
        it "can parse TOML files" $
           parseToml tomlStr1 toml1
        it "can parse mix of tables and arrays" $
           parseToml tomlStr2 toml2
      where
        tomlStr1, tomlStr2 :: Text
        tomlStr1 = T.unlines
            [ " # This is a TOML document.\n\n"
            , "title = \"TOML Example\" # Comment \n\n"
            , "[owner]\n"
            , "  name = \"Tom Preston-Werner\" "
            , "  enabled = true # First class dates"
            ]
        tomlStr2 = T.unlines
            [ "[[array1]]\n key1 = \"some string\" \n"
            , ""
            , "[table1]  \n key2 = 123 \n"
            , "[[array2]]\n key3 = 3.14 \n"
            , "  [table2]  \n key4 = true"
            ]

        toml1, toml2 :: TOML
        toml1 = mkToml $ do
            "title" =: "TOML Example"
            table "owner" $ do
                "name" =: "Tom Preston-Werner"
                "enabled" =: Bool True

        toml2 = mkToml $ do
            tableArray "array1" $
                "key1" =: "some string" :| []
            table "table1" $ "key2" =: 123
            tableArray "array2" $
                "key3" =: Double 3.14 :| []
            table "table2" $ "key4" =: Bool True
