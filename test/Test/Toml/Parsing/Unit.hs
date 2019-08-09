{-# LANGUAGE FlexibleContexts #-}

module Test.Toml.Parsing.Unit where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay (..), TimeZone, ZonedTime (..), fromGregorian,
                  minutesToTimeZone)
import Test.Hspec.Megaparsec (parseSatisfies, shouldFailOn, shouldParse)
import Test.Tasty.Hspec (Expectation, Spec, context, describe, it)
import Text.Megaparsec (Parsec, ShowErrorComponent, Stream, parse)

import Toml.Edsl (mkToml, table, tableArray, (=:))
import Toml.Parser.String (textP)
import Toml.Parser.TOML (keyP, tomlP)
import Toml.Parser.Value (arrayP, boolP, dateTimeP, doubleP, integerP)
import Toml.PrefixTree (Key (..), Piece (..), fromList)
import Toml.Type (AnyValue (..), TOML (..), UValue (..), Value (..))

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

spec_Parser :: Spec
spec_Parser = do
    arraySpecs
    boolSpecs
    doubleSpecs
    integerSpecs
    keySpecs
    textSpecs
    dateSpecs
    tomlSpecs

arraySpecs :: Spec
arraySpecs = describe "arrayP" $ do
    it "can parse arrays" $ do
        parseArray "[]"              []
        parseArray "[1]"             [int1]
        parseArray "[1, 2, 3]"       [int1, int2, int3]
        parseArray "[1.2, 2.3, 3.4]" [UDouble 1.2, UDouble 2.3, UDouble 3.4]
        parseArray "['x', 'y']"      [UText "x", UText "y"]
        parseArray "[[1], [2]]" [UArray [UInteger 1], UArray [UInteger 2]]
        parseArray "[1920-12-10, 1979-05-27]" [UDay  day2, UDay day1]
        parseArray "[16:33:05, 10:15:30]" [UHours (TimeOfDay 16 33 5), UHours (TimeOfDay 10 15 30)]
    it "can parse multiline arrays" $
        parseArray "[\n1,\n2\n]" [int1, int2]
    it "can parse an array of arrays" $
        parseArray "[[1], [2.3, 5.1]]" [UArray [int1], UArray [UDouble 2.3, UDouble 5.1]]
    it "can parse an array with terminating commas (trailing commas)" $ do
        parseArray "[1, 2,]"        [int1, int2]
        parseArray "[1, 2, 3, , ,]" [int1, int2, int3]
    it "allows an arbitrary number of comments and newlines before or after a value" $
        parseArray "[\n\n#c\n1, #c 2 \n 2, \n\n\n 3, #c \n #c \n 4]" [int1, int2, int3, int4]
    it "ignores white spaces" $
        parseArray "[   1    ,    2,3,  4      ]" [int1, int2, int3, int4]
    it "fails if the elements are not surrounded by square brackets" $ do
        arrayFailOn "1, 2, 3"
        arrayFailOn "[1, 2, 3"
        arrayFailOn "1, 2, 3]"
        arrayFailOn "{'x', 'y', 'z'}"
        arrayFailOn "(\"ab\", \"cd\")"
        arrayFailOn "<true, false>"
    it "fails if the elements are not separated by commas" $ do
        arrayFailOn "[1 2 3]"
        arrayFailOn "[1 . 2 . 3]"
        arrayFailOn "['x' - 'y' - 'z']"
        arrayFailOn "[1920-12-10, 10:15:30]"

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

doubleSpecs :: Spec
doubleSpecs = describe "doubleP" $ do
    it "can parse a number which consists of an integral part, and a fractional part" $ do
        parseDouble "+1.0"      1.0
        parseDouble "3.1415"    3.1415
        parseDouble "0.0"       0.0
        parseDouble "-0.01"     (-0.01)
        parseDouble "5e+22"     5e+22
        parseDouble "1e6"       1e6
        parseDouble "-2E-2"     (-2E-2)
        parseDouble "6.626e-34" 6.626e-34
    it "can parse a number with underscores" $ do
        parseDouble "5e+2_2"         5e+22
        parseDouble "1.1_1e6"        1.11e6
        parseDouble "-2_2.21_9E-0_2" (-22.219E-2)
    it "can parse sign-prefixed zero" $ do
        parseDouble "+0.0" 0.0
        parseDouble "-0.0" (-0.0)
    it "can parse positive and negative special float values (inf and nan)" $ do
        parseDouble "inf"  (1 / 0)
        parseDouble "+inf" (1 / 0)
        parseDouble "-inf" (-1 / 0)
        doubleSatisfies "nan"  isNaN
        doubleSatisfies "+nan" isNaN
        doubleSatisfies "-nan" isNaN
    it "fails if `inf` or `nan` are not all lowercase" $ do
        doubleFailOn "Inf"
        doubleFailOn "INF"
        doubleFailOn "Nan"
        doubleFailOn "NAN"
        doubleFailOn "NaN"
  where
    doubleSatisfies given f = parse doubleP "" given `parseSatisfies` f

integerSpecs :: Spec
integerSpecs = describe "integerP" $ do
    context "when the integer is in decimal representation" $ do
        it "can parse positive integer numbers" $ do
            parseInteger "10" 10
            parseInteger "+3" 3
            parseInteger "0"  0
        it "can parse negative integer numbers" $
            parseInteger "-123" (-123)
        it "can parse sign-prefixed zero as an unprefixed zero" $ do
            parseInteger "+0" 0
            parseInteger "-0" 0
        it "can parse both the minimum and maximum numbers in the 64 bit range" $ do
            parseInteger "-9223372036854775808" (-9223372036854775808)
            parseInteger "9223372036854775807"  9223372036854775807
        it "can parse numbers with underscores between digits" $ do
            parseInteger "1_000" 1000
            parseInteger "5_349_221" 5349221
            parseInteger "1_2_3_4_5" 12345
        it "does not parse incorrect underscores" $ do
            integerFailOn "1_2_3_"
            integerFailOn "13_"
            integerFailOn "_123_"
            integerFailOn "_13"
            integerFailOn "_"
        it "does not parse numbers with leading zeros" $ do
            parseInteger "0123" 0
            parseInteger "-023" 0
    context "when the integer is in binary representation" $ do
        it "can parse numbers prefixed with `0b`" $ do
            parseInteger "0b1101" 13
            parseInteger "0b0"    0
        it "does not parse numbers prefixed with `0B`" $
            parseInteger "0B1101" 0
        it "can parse numbers with leading zeros after the prefix" $ do
            parseInteger "0b000"   0
            parseInteger "0b00011" 3
        it "does not parse negative numbers" $
            parseInteger "-0b101" 0
        it "does not parse numbers with non-valid binary digits" $
            parseInteger "0b123" 1
    context "when the integer is in octal representation" $ do
        it "can parse numbers prefixed with `0o`" $ do
            parseInteger "0o567" 0o567
            parseInteger "0o0"   0
        it "does not parse numbers prefixed with `0O`" $
            parseInteger "0O567" 0
        it "can parse numbers with leading zeros after the prefix" $ do
            parseInteger "0o000000" 0
            parseInteger "0o000567" 0o567
        it "does not parse negative numbers" $
            parseInteger "-0o123" 0
        it "does not parse numbers with non-valid octal digits" $
            parseInteger "0o789" 0o7
    context "when the integer is in hexadecimal representation" $ do
        it "can parse numbers prefixed with `0x`" $ do
            parseInteger "0x12af" 0x12af
            parseInteger "0x0"    0
        it "does not parse numbers prefixed with `0X`" $
            parseInteger "0Xfff" 0
        it "can parse numbers with leading zeros after the prefix" $ do
            parseInteger "0x00000" 0
            parseInteger "0x012af" 0x12af
        it "does not parse negative numbers" $
            parseInteger "-0xfff" 0
        it "does not parse numbers with non-valid hexadecimal digits" $
            parseInteger "0xfgh" 0xf
        it "can parse numbers when hex digits are lowercase" $
            parseInteger "0xabcdef" 0xabcdef
        it "can parse numbers when hex digits are uppercase" $
            parseInteger "0xABCDEF" 0xABCDEF
        it "can parse numbers when hex digits are in both lowercase and uppercase" $ do
            parseInteger "0xAbCdEf" 0xAbCdEf
            parseInteger "0xaBcDeF" 0xaBcDeF

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

textSpecs :: Spec
textSpecs = describe "textP" $ do
    context "when the string is a basic string" $ do
        it "can parse strings surrounded by double quotes" $ do
            parseText (dquote "xyz") "xyz"
            parseText (dquote "")    ""
            textFailOn "\"xyz"
            textFailOn "xyz\""
            textFailOn "xyz"
        it "can parse escaped quotation marks, backslashes, and control characters" $ do
            parseText (dquote "backspace: \\b") "backspace: \b"
            parseText (dquote "tab: \\t")       "tab: \t"
            parseText (dquote "linefeed: \\n")  "linefeed: \n"
            parseText (dquote "form feed: \\f") "form feed: \f"
            parseText (dquote "carriage return: \\r") "carriage return: \r"
            parseText (dquote "quote: \\\"")     "quote: \""
            parseText (dquote "backslash: \\\\") "backslash: \\"
            parseText (dquote "a\\uD7FFxy\\U0010FFFF\\uE000") "a\55295xy\1114111\57344"
        it "fails if the string has an unescaped backslash, or control character" $ do
            textFailOn (dquote "new \n line")
            textFailOn (dquote "back \\ slash")
        it "fails if the string has an escape sequence that is not listed in the TOML specification" $
            textFailOn (dquote "xy\\z \\abc")
        it "fails if the string is not on a single line" $ do
            textFailOn (dquote "\nabc")
            textFailOn (dquote "ab\r\nc")
            textFailOn (dquote "abc\n")
        it "fails if escape codes are not valid Unicode scalar values" $ do
            textFailOn (dquote "\\u1")
            textFailOn (dquote "\\uxyzw")
            textFailOn (dquote "\\U0000")
            textFailOn (dquote "\\uD8FF")
            textFailOn (dquote "\\U001FFFFF")
    context "when the string is a multi-line basic string" $ do

        it "can parse multi-line strings surrounded by three double quotes" $
            parseText (dquote3 "Roses are red\nViolets are blue")
                "Roses are red\nViolets are blue"
        it "can parse single-line strings surrounded by three double quotes" $
            parseText (dquote3 "Roses are red Violets are blue")
                "Roses are red Violets are blue"
        it "can parse all of the escape sequences that are valid for basic strings" $ do
            parseText (dquote3 "backspace: \\b") "backspace: \b"
            parseText (dquote3 "tab: \\t")       "tab: \t"
            parseText (dquote3 "linefeed: \\n")  "linefeed: \n"
            parseText (dquote3 "form feed: \\f") "form feed: \f"
            parseText (dquote3 "carriage return: \\r") "carriage return: \r"
            parseText (dquote3 "quote: \\\"")     "quote: \""
            parseText (dquote3 "backslash: \\\\") "backslash: \\"
            parseText (dquote3 "a\\uD7FFxy\\U0010FFFF\\uE000") "a\55295xy\1114111\57344"
        it "does not ignore whitespaces or newlines" $
            parseText (dquote3 "\nabc  \n   xyz") "abc  \n   xyz"
        it "ignores a newline only if it immediately follows the opening delimiter" $
            parseText (dquote3 "\nThe quick brown") "The quick brown"
        it "ignores whitespaces and newlines after line ending backslash" $
            parseText (dquote3 "The quick brown \\\n\n   fox jumps over") "The quick brown fox jumps over"
        it "fails if the string has an unescaped backslash, or control character" $ do
            textFailOn (dquote3 "backslash \\ .")
            textFailOn (dquote3 "backspace \b ..")
            textFailOn (dquote3 "tab \t ..")
    context "when the string is a literal string" $ do
        it "can parse strings surrounded by single quotes" $ do
            parseText (squote "C:\\Users\\nodejs\\templates")
                      "C:\\Users\\nodejs\\templates"
            parseText (squote "\\\\ServerX\\admin$\\system32\\")
                      "\\\\ServerX\\admin$\\system32\\"
            parseText (squote "Tom \"Dubs\" Preston-Werner")
                      "Tom \"Dubs\" Preston-Werner"
            parseText (squote "<\\i\\c*\\s*>") "<\\i\\c*\\s*>"
            parseText (squote "a \t tab")      "a \t tab"
        it "fails if the string is not on a single line" $ do
            textFailOn (squote "\nabc")
            textFailOn (squote "ab\r\nc")
            textFailOn (squote "abc\n")
    context "when the string is a multi-line literal string" $ do
        it "can parse multi-line strings surrounded by three single quotes" $
            parseText (squote3 "first line \nsecond.\n   3\n")
                "first line \nsecond.\n   3\n"
        it "can parse single-line strings surrounded by three single quotes" $
            parseText (squote3 "I [dw]on't need \\d{2} apples")
                "I [dw]on't need \\d{2} apples"
        it "ignores a newline immediately following the opening delimiter" $
            parseText (squote3 "\na newline \nsecond.\n   3\n")
                "a newline \nsecond.\n   3\n"
        it "fails if the string has an unescaped control character other than tab" $ do
            parseText (squote3 "\t") "\t"
            textFailOn (squote3 "\b")

dateSpecs :: Spec
dateSpecs = describe "dateTimeP" $ do
    it "can parse a date-time with an offset" $ do
        parseDateTime "1979-05-27T07:32:00Z" $ makeZoned day1 hours1 offset0
        parseDateTime "1979-05-27T00:32:00+07:10" $
            makeZoned day1 (TimeOfDay 0 32 0) offset710
        parseDateTime "1979-05-27T00:32:00.999999-07:25" $
            makeZoned day1 (TimeOfDay 0 32 0.999999) (makeOffset (-7) 25)
    it "can parse a date-time with an offset when the T delimiter is replaced with a space" $
        parseDateTime "1979-05-27 07:32:00Z" $ makeZoned day1 hours1 offset0
    it "can parse a date-time without an offset" $ do
        parseDateTime "1979-05-27T17:32:00"
            (ULocal $ LocalTime day1 (TimeOfDay 17 32 0))
        parseDateTime "1979-05-27T00:32:00.999999"
            (ULocal $ LocalTime day1 (TimeOfDay 0 32 0.999999))
    it "can parse a local date"
        $ parseDateTime "1979-05-27" (UDay day1)
    it "can parse a local time" $ do
        parseDateTime "07:32:00"        (UHours hours1)
        parseDateTime "00:32:00.999999" (UHours $ TimeOfDay 0 32 0.999999)
    it "truncates the additional precision after picoseconds in the fractional seconds" $
        parseDateTime "00:32:00.99999999999199" (UHours $ TimeOfDay 0 32 0.999999999991)
    it "fails if the date is not valid" $ do
        dateTimeFailOn "1920-15-12"
        dateTimeFailOn "1920-12-40"
    it "fails if the date does not have the form: 'yyyy-mm-dd'" $ do
        dateTimeFailOn "1920-01-1"
        dateTimeFailOn "1920-1-01"
        dateTimeFailOn "920-01-01"
        dateTimeFailOn "1920/10/01"
    it "fails if the time is not valid" $ do
        dateTimeFailOn "25:10:10"
        dateTimeFailOn "10:70:10"
        dateTimeFailOn "10:10:70"
    it "fails if the time does not have the form: 'hh:mm:ss'" $ do
        dateTimeFailOn "1:12:12"
        dateTimeFailOn "12:1:12"
        dateTimeFailOn "12:12:1"
        dateTimeFailOn "12-12-12"
    it "fails if the offset does not have any of the forms: 'Z', '+hh:mm', '-hh:mm'" $ do
        parseDateTime "1979-05-27T00:32:00X"
            (ULocal $ LocalTime day1 (TimeOfDay 0 32 0))
        parseDateTime "1979-05-27T00:32:00+07:1"
            (ULocal $ LocalTime day1 (TimeOfDay 0 32 0))
        parseDateTime "1979-05-27T00:32:00+7:01"
            (ULocal $ LocalTime day1 (TimeOfDay 0 32 0))
        parseDateTime "1979-05-27T00:32:0007:00"
            (ULocal $ LocalTime day1 (TimeOfDay 0 32 0))

tomlSpecs :: Spec
tomlSpecs = do
    describe "Key/values" $ do
        it "can parse key/value pairs" $ do
            parseToml "x='abcdef'" (tomlFromKeyVal [(makeKey ["x"], AnyValue (Text "abcdef"))])
            parseToml "x=1"        (tomlFromKeyVal [(makeKey ["x"], AnyValue (Integer 1))])
            parseToml "x=5.2"      (tomlFromKeyVal [(makeKey ["x"], AnyValue (Double 5.2))])
            parseToml "x=true"     (tomlFromKeyVal [(makeKey ["x"], AnyValue (Bool True))])
            parseToml "x=[1, 2, 3]" (tomlFromKeyVal [(makeKey ["x"] , AnyValue (Array [Integer 1, Integer 2, Integer 3]))])
            parseToml "x = 1920-12-10" (tomlFromKeyVal [(makeKey ["x"], AnyValue (Day day2))])
        it "ignores white spaces around key names and values" $ do
            let toml = tomlFromKeyVal [(makeKey ["x"], AnyValue (Integer 1))]
            parseToml "x=1    "   toml
            parseToml "x=    1"   toml
            parseToml "x    =1"   toml
            parseToml "x\t= 1 "   toml
            parseToml "\"x\" = 1" (tomlFromKeyVal [(makeKey [dquote "x"], AnyValue (Integer 1))])
        --xit "fails if the key, equals sign, and value are not on the same line" $ do
        --  keyValFailOn "x\n=\n1"
        --  keyValFailOn "x=\n1"
        --  keyValFailOn "\"x\"\n=\n1"
        it "works if the value is broken over multiple lines" $
            parseToml "x=[1, \n2\n]" (tomlFromKeyVal [(makeKey ["x"], AnyValue (Array [Integer 1, Integer 2]))])
        it "fails if the value is not specified" $
            tomlFailOn "x="

    describe "tables" $ do
        it "can parse a TOML table" $
          parseToml "[table] \n key1 = \"some string\"\nkey2 = 123"
                $ tomlFromTable [(makeKey ["table"], tomlFromKeyVal [str, int])]
        it "can parse an empty TOML table" $
            parseToml "[table]" (tomlFromTable [(makeKey ["table"], mempty)])
        it "can parse a table with subarrays" $ do
            let t = tomlFromTable [(makeKey ["table"], tomlFromArray [(makeKey ["array"], strT :| [intT])])]
            parseToml "[table] \n [[table.array]] \nkey1 = \"some string\"\n \
                                  \[[table.array]] \nkey2 = 123" t
        it "can parse a TOML inline table" $
            parseToml "table={key1 = \"some string\", key2 = 123}"
                (tomlFromTable [(makeKey ["table"], tomlFromKeyVal [str, int])])
        it "can parse an empty inline TOML table" $
            parseToml "table = {}" (tomlFromTable [(makeKey ["table"], mempty)])
        it "can parse a table followed by an inline table" $
            parseToml "[table1] \n  key1 = \"some string\" \n table2 = {key2 = 123}"
                (tomlFromTable [(makeKey ["table1"], tomlFromKeyVal [str])
                               ,(makeKey ["table2"], tomlFromKeyVal [int])])
        it "can parse an empty table followed by an inline table" $
            parseToml "[table1] \n table2 = {key2 = 123}"
                (tomlFromTable [(makeKey ["table1"], mempty)
                               ,(makeKey ["table2"], tomlFromKeyVal [int])])
        it "allows the name of the table to be any valid TOML key" $ do
            parseToml "dog.\"tater.man\"={}"
                (tomlFromTable [(makeKey ["dog", dquote "tater.man"], mempty)])
            parseToml "j.\"ʞ\".'l'={}"
                (tomlFromTable [(makeKey ["j", dquote "ʞ", squote "l"], mempty)])

    describe "array of tables" $ do
        it "can parse an empty array" $
            parseToml "[[array]]" (tomlFromArray [(makeKey ["array"], mempty :| [])])
        it "can parse an array of key/values" $ do
            let array = tomlFromArray [(makeKey ["array"], NE.fromList [strT, intT])]
            parseToml "[[array]]\n key1 = \"some string\"\n \
                      \[[array]]\n key2 = 123" array
        it "can parse an array of tables" $ do
            let table1 = tomlFromTable [(makeKey ["table1"], strT)]
                table2 = tomlFromTable [(makeKey ["table2"], intT)]
                array  = tomlFromArray [(makeKey ["array"], NE.fromList [table1, table2])]
            parseToml "[[array]]\n[array.table1] \n key1 = \"some string\"\n \
                      \[[array]]\n[array.table2] \n key2 = 123" array
        it "can parse an array of array" $ do
            let arr = tomlFromArray [(makeKey ["subarray"], NE.fromList [strT, intT])]
                array = tomlFromArray [(makeKey ["array"], arr :| [])]
            parseToml "[[array]] \n [[array.subarray]] \nkey1 = \"some string\"\n \
                      \[[array.subarray]] \nkey2 = 123" array
        it "can parse an array of arrays" $ do
            let arr1 = (makeKey ["table-1"], strT :| [])
                arr2 = (makeKey ["table-2"], intT :| [])
                array = tomlFromArray [(makeKey ["array"], tomlFromArray [arr1, arr2] :| [])]
            parseToml "[[array]]\n [[array.table-1]] \nkey1 = \"some string\"\n \
                                  \[[array.table-2]] \nkey2 = 123" array
        it "can parse very large arrays" $ do
            let array = tomlFromArray [(makeKey ["array"], NE.fromList $ replicate 1000 mempty)]
            parseToml (mconcat $ replicate 1000 "[[array]]\n") array
        it "can parse an inline array of tables" $ do
            let array  = tomlFromArray [(makeKey ["table"], NE.fromList [strT, intT])]
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

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

parseX :: (ShowErrorComponent e, Stream s, Show a, Eq a)
       => Parsec e s a -> s -> a -> Expectation
parseX p given expected = parse p "" given `shouldParse` expected

failOn :: Show a => Parsec e s a -> s -> Expectation
failOn p given = parse p "" `shouldFailOn` given

parseArray :: Text -> [UValue] -> Expectation
parseArray = parseX arrayP
parseBool :: Text -> Bool -> Expectation
parseBool = parseX boolP
parseDateTime :: Text -> UValue -> Expectation
parseDateTime = parseX dateTimeP
parseDouble :: Text -> Double -> Expectation
parseDouble = parseX doubleP
parseInteger :: Text -> Integer -> Expectation
parseInteger = parseX integerP
parseKey :: Text -> Key -> Expectation
parseKey = parseX keyP
parseText :: Text -> Text -> Expectation
parseText = parseX textP

parseToml :: Text -> TOML -> Expectation
parseToml = parseX tomlP

arrayFailOn, boolFailOn, dateTimeFailOn, doubleFailOn, integerFailOn, textFailOn, tomlFailOn :: Text -> Expectation
arrayFailOn     = failOn arrayP
boolFailOn      = failOn boolP
dateTimeFailOn  = failOn dateTimeP
doubleFailOn    = failOn doubleP
integerFailOn   = failOn integerP
textFailOn      = failOn textP
tomlFailOn      = failOn tomlP

-- UValue Util

makeZoned :: Day -> TimeOfDay -> TimeZone -> UValue
makeZoned d h offset = UZoned $ ZonedTime (LocalTime d h) offset

makeOffset :: Int -> Int -> TimeZone
makeOffset hours mins = minutesToTimeZone (hours * 60 + mins * signum hours)

makeKey :: [Text] -> Key
makeKey = Key . NE.fromList . map Piece

tomlFromKeyVal :: [(Key, AnyValue)] -> TOML
tomlFromKeyVal kv = TOML (HashMap.fromList kv) mempty mempty mempty

tomlFromTable :: [(Key, TOML)] -> TOML
tomlFromTable t = TOML mempty (fromList t) mempty mempty

tomlFromArray :: [(Key, NonEmpty TOML)] -> TOML
tomlFromArray a = TOML mempty mempty (HashMap.fromList a) mempty

-- Surround given text with quotes.
quoteWith :: Text -> Text -> Text
quoteWith q t = q <> t <> q
squote, dquote, squote3, dquote3 :: Text -> Text
squote = quoteWith "'"
dquote = quoteWith "\""
squote3 = quoteWith "'''"
dquote3 = quoteWith "\"\"\""

-- Test Data

-- Key-Value pairs
str, int :: (Key, AnyValue)
str = (makeKey ["key1"], AnyValue (Text "some string"))
int = (makeKey ["key2"], AnyValue (Integer 123))

strT, intT :: TOML
strT = tomlFromKeyVal [str]
intT = tomlFromKeyVal [int]

int1, int2, int3, int4 :: UValue
int1 = UInteger 1
int2 = UInteger 2
int3 = UInteger 3
int4 = UInteger 4

offset0, offset710 :: TimeZone
offset0 = makeOffset 0 0
offset710 = makeOffset 7 10

day1, day2 :: Day
day1 = fromGregorian 1979 5 27  -- 1979-05-27
day2 = fromGregorian 1920 12 10 -- 1920-12-10

hours1 :: TimeOfDay
hours1 = TimeOfDay 7 32 0  -- 07:32:00
