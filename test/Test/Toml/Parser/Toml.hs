{-# LANGUAGE PatternSynonyms #-}

module Test.Toml.Parser.Toml
    ( tomlSpecs
    ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it)

import Test.Toml.Parser.Common (day2, failOn, parseToml, tomlFailOn)
import Toml.Parser.Item (keyValP)
import Toml.Type.Edsl (empty, mkToml, table, tableArray, (=:))
import Toml.Type.Key (pattern (:||))
import Toml.Type.TOML (TOML (..))
import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Value (Value (..), array)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T


tomlSpecs :: Spec
tomlSpecs = do
    describe "Key/values" $ do
        it "can parse key/value pairs" $ do
            parseToml "x='abcdef'" $ mkToml ("x" =: "abcdef")
            parseToml "x= 1"  $ mkToml ("x" =: 1)
            parseToml "x =5.2" $ mkToml ("x" =: Double 5.2)
            parseToml "x = true" $ mkToml ("x" =: Bool True)
            parseToml "x= [1, 2, 3]" $ mkToml ("x" =: array [1, 2, 3])
            parseToml "x =1920-12-10" $ mkToml ("x" =: Day day2)
        it "ignores white spaces around key names and values" $ do
            let toml = mkToml ("x" =: 1)
            parseToml "x=1    "   toml
            parseToml "x=    1"   toml
            parseToml "x    =1"   toml
            parseToml "x\t= 1 "   toml
            parseToml "\"x\" = 1" $ mkToml ("\"x\"" =: 1)
        it "fails if the key, equals sign, and value are not on the same line" $ do
            failOn keyValP "x\n=\n1"
            failOn keyValP "x=\n1"
            failOn keyValP "\"x\"\n=\n1"
        it "works if the value is broken over multiple lines" $
            parseToml "x=[1, \n2\n]" $ mkToml ("x" =: array [1, 2])
        it "can parse arrays with elements of different types" $
            parseToml "x = [1, \"a\", 2.5]" $
                mkToml ("x" =: Array [AnyValue (Integer 1), AnyValue (Text "a"), AnyValue (Double 2.5)])
        it "fails if the value is not specified" $
            tomlFailOn "x="
        it "fails if there is no newline between key/value pairs" $ do
            tomlFailOn "a = 1 b = 2"
            tomlFailOn "a = \"x\" b = \"y\""
            tomlFailOn "0=0r=false"
        it "allows comments and CRLF line endings after key/value pairs" $ do
            parseToml "x = 1 # comment\r\ny = 2\r\n" $ mkToml ("x" =: 1 >> "y" =: 2)
            parseToml "# only a comment" $ mkToml empty
            parseToml "x = 1 #\tcomment with a tab" $ mkToml ("x" =: 1)
        it "fails on control characters in comments or on a bare carriage return" $ do
            tomlFailOn "x = 1 # \SOH"
            tomlFailOn "x = 1 # \DEL"
            tomlFailOn "x = 1 # \r y = 2"
            tomlFailOn "x = 1\r"
            tomlFailOn "\r"
            tomlFailOn "x = 1\v"

    describe "tables" $ do
        it "can parse a TOML table" $ do
            let t  = mkToml $
                        table "table" $ do
                            "key1" =: "some string"
                            "key2" =: 123

            parseToml "[table] \n key1 = \"some string\"\nkey2 = 123" t
        it "can parse an empty TOML table" $
            parseToml "[table]" $ mkToml (table "table" empty)
        it "ignores whitespace inside table headers" $ do
            parseToml "[ table ]" $ mkToml (table "table" empty)
            parseToml "[ a . b ]" $ mkToml (table "a.b" empty)
            parseToml "[[ a . b ]]" $ mkToml (tableArray "a.b" (empty :| []))
        it "fails if a table header is not on a single line" $ do
            tomlFailOn "[tbl\n]"
            tomlFailOn "[tbl\n.sub]"
            tomlFailOn "[tbl] key = 1"
            tomlFailOn "[[arr]] key = 1"
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
        it "can parse an inline table spanning multiple lines" $ do
            let t = mkToml $ table "t" $ do
                    "a" =: 1
                    "b" =: 2
            parseToml "t = {\n  a = 1, # comment\n  b = 2,\n}" t
            parseToml "t = {#comment\n\ta = 1,#comment\n\tb = 2#comment\n}#comment" t
            parseToml "t = { a = 1, b = 2, }" t
            parseToml "t = {\n}" $ mkToml (table "t" empty)
        it "fails on consecutive commas in an inline table" $ do
            tomlFailOn "t = { a = 1,, }"
            tomlFailOn "t = { , }"
        it "can parse nested inline tables" $ do
            parseToml "t = { a = { b = {} } }" $
                mkToml $ table "t" $ table "a" $ table "b" empty
            parseToml "t = { a = { b = 1 }, c = 2 }" $
                mkToml $ table "t" $ do
                    table "a" $ "b" =: 1
                    "c" =: 2
            parseToml "t = [ { a = { b = 1 } } ]" $
                mkToml $ tableArray "t" $ table "a" ("b" =: 1) :| []
            parseToml "t = { a = [ { b = 1 }, { b = 2 } ] }" $
                mkToml $ table "t" $ tableArray "a" $ "b" =: 1 :| ["b" =: 2]
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
            parseToml "dog.\"tater.man\"={}" $ mkToml $ table ("dog" :|| ["tater.man"]) empty
            parseToml "j.\"ʞ\".'l'={}" $ mkToml $ table "j.\"ʞ\".'l'" empty

    describe "array of tables" $ do
        it "can parse an empty array" $
            parseToml "[[array]]" $ mkToml $ tableArray "array" (empty :| [])
        it "can parse an array of key/values" $ do
            let arr = mkToml $
                        tableArray "array" $
                            "key1" =: "some string" :|
                            ["key2" =: 123]

            parseToml "[[array]]\n key1 = \"some string\"\n \
                       \[[array]]\n key2 = 123" arr
        it "can parse an array of tables" $ do
            let table1 = table "table1" ("key1" =: "some string")
                table2 = table "table2" ("key2" =: 123)
                arr = mkToml $ tableArray "array" $ table1 :| [table2]

            parseToml "[[array]]\n[array.table1] \n key1 = \"some string\"\n \
                       \[[array]]\n[array.table2] \n key2 = 123" arr
        it "can parse an array of array" $ do
            let sub = tableArray "subarray" ("key1" =: "some string" :| ["key2" =: 123])
                arr = mkToml $ tableArray "array" (sub :| [])

            parseToml "[[array]] \n [[array.subarray]] \nkey1 = \"some string\"\n \
                       \[[array.subarray]] \nkey2 = 123" arr
        it "can parse an array of arrays" $ do
            let
                arr1 = tableArray "table-1" ("key1" =: Text "some string" :| [])
                arr2 = tableArray "table-2" ("key2" =: Integer 123 :| [])
                arr = mkToml $ tableArray "array" $ (arr1 >> arr2) :| []

            parseToml "[[array]]\n [[array.table-1]] \nkey1 = \"some string\"\n \
                                     \[[array.table-2]] \nkey2 = 123" arr
        it "can parse very large arrays" $ do
            let arr = mkToml $ tableArray "array" $ NE.fromList $ replicate 1000 empty
            parseToml (mconcat $ replicate 1000 "[[array]]\n") arr
        it "can parse an inline array of tables" $ do
            let arr = mkToml $ tableArray "table" $ NE.fromList ["key1" =: "some string", "key2" =: 123]
            parseToml "table = [{key1 = \"some string\"}, {key2 = 123}]" arr

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
