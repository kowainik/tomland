{-# LANGUAGE FlexibleContexts #-}

module Test.Toml.Parsing.Unit where

import Data.Semigroup ((<>))
import Test.Hspec.Megaparsec (parseSatisfies, shouldFailOn, shouldParse)
import Test.Tasty.Hspec (Spec, context, describe, it)
import Text.Megaparsec (parse)

import Toml.Parser (arrayP, boolP, doubleP, integerP, keyP, keyValP, tableHeaderP, textP, tomlP)
import Toml.PrefixTree (Key (..), Piece (..), fromList)
import Toml.Type (AnyValue (..), TOML (..), UValue (..), Value (..))

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List.NonEmpty as NE

spec_Parser :: Spec
spec_Parser = do
  let parseX p given expected   = parse p "" given `shouldParse` expected
      failOn p given            = parse p "" `shouldFailOn` given
      parseXSatisfies p given f = parse p "" given `parseSatisfies` f

      parseArray   = parseX arrayP
      parseBool    = parseX boolP
      parseDouble  = parseX doubleP
      parseInteger = parseX integerP
      parseKey     = parseX keyP
      parseKeyVal  = parseX keyValP
      parseText    = parseX textP
      parseTable   = parseX tableHeaderP
      parseToml    = parseX tomlP

      arrayFailOn  = failOn arrayP
      boolFailOn   = failOn boolP
      doubleFailOn = failOn doubleP
      keyValFailOn = failOn keyValP
      textFailOn   = failOn textP

      doubleSatisfies = parseXSatisfies doubleP

      quoteWith q t = q <> t <> q
      squote        = quoteWith "'"
      dquote        = quoteWith "\""

      makeKey k = (Key . NE.fromList) (map Piece k)

      tomlFromList kv = TOML (HashMap.fromList kv) mempty


  describe "arrayP" $ do
    it "can parse arrays" $ do
      parseArray "[]" []
      parseArray "[1]" [UInteger 1]
      parseArray "[1, 2, 3]" [UInteger 1, UInteger 2, UInteger 3]
      parseArray "[1.2, 2.3, 3.4]" [UDouble 1.2, UDouble 2.3, UDouble 3.4]
      parseArray "['x', 'y']" [UText "x", UText "y"]
      parseArray "[[1], [2]]" [UArray [UInteger 1], UArray [UInteger 2]]
    --xit "can parse arrays of dates" $ do
    --  let makeDay   y m d = (UDate . Day) (fromGregorian y m d)
    --      makeHours h m s = (UDate . Hours) (TimeOfDay h m s)
    --  parseArray "[1920-12-10, 10:15:30]" [makeDay 1920 12 10, makeHours 10 15 30]
    it "can parse multiline arrays" $ do
      parseArray "[\n1,\n2\n]" [UInteger 1, UInteger 2]
    it "can parse an array of arrays" $ do
      parseArray "[[1], [2.3, 5.1]]" [UArray [UInteger 1], UArray [UDouble 2.3, UDouble 5.1]]
    it "can parse an array with terminating commas (trailing commas)" $ do
      parseArray "[1, 2,]" [UInteger 1, UInteger 2]
      parseArray "[1, 2, 3, , ,]" [UInteger 1, UInteger 2, UInteger 3]
    it "allows an arbitrary number of comments and newlines before or after a value" $ do
      parseArray "[\n\n#c\n1, #c 2 \n 2, \n\n\n 3, #c \n #c \n 4]" [UInteger 1, UInteger 2, UInteger 3, UInteger 4]
    it "ignores white spaces" $ do
      parseArray "[   1    ,    2,3,  4      ]" [UInteger 1, UInteger 2, UInteger 3, UInteger 4]
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

  describe "boolP" $ do
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

  describe "doubleP" $ do
      it "can parse a number which consists of an integral part, and a fractional part" $ do
        parseDouble "+1.0" 1.0
        parseDouble "3.1415" 3.1415
        parseDouble "0.0" 0.0
        parseDouble "-0.01" (-0.01)
      it "can parse a number which consists of an integral part, and an exponent part" $ do
        parseDouble "5e+22" 5e+22
        parseDouble "1e6" 1e6
        parseDouble "-2E-2" (-2E-2)
      it "can parse a number which consists of an integral, a fractional, and an exponent part" $ do
        parseDouble "6.626e-34" 6.626e-34
      it "can parse sign-prefixed zero" $ do
        parseDouble "+0.0" 0.0
        parseDouble "-0.0" (-0.0)
      it "can parse positive and negative special float values (inf and nan)" $ do
        parseDouble "inf" (1 / 0)
        parseDouble "+inf" (1 / 0)
        parseDouble "-inf" (-1 / 0)
        doubleSatisfies "nan" isNaN
        doubleSatisfies "+nan" isNaN
        doubleSatisfies "-nan" isNaN
      it "fails if `inf` or `nan` are not all lowercase" $ do
        doubleFailOn "Inf"
        doubleFailOn "INF"
        doubleFailOn "Nan"
        doubleFailOn "NAN"
        doubleFailOn "NaN"

  describe "integerP" $ do
    context "when the integer is in decimal representation" $ do
      it "can parse positive integer numbers" $ do
        parseInteger "10" 10
        parseInteger "+3" 3
        parseInteger "0" 0
      it "can parse negative integer numbers" $ do
        parseInteger "-123" (-123)
      it "can parse sign-prefixed zero as an unprefixed zero" $ do
        parseInteger "+0" 0
        parseInteger "-0" 0
      it "can parse both the minimum and maximum numbers in the 64 bit range" $ do
        parseInteger "-9223372036854775808" (-9223372036854775808)
        parseInteger "9223372036854775807" 9223372036854775807
      --xit "can parse numbers with underscores between digits" $ do
      --  parseInt "1_000" 1000
      --  parseInt "5_349_221" 5349221
      --  parseInt "1_2_3_4_5" 12345
      --  parseInt "1_2_3_" 1
      --  parseInt "13_" 13
      --  intFailOn "_123_"
      --  intFailOn "_13"
      --  intFailOn "_"
      --xit "does not parse numbers with leading zeros" $ do
      --  parseInt "0123" 0
      --  parseInt "-023" 0
    context "when the integer is in binary representation" $ do
      it "can parse numbers prefixed with `0b`" $ do
        parseInteger "0b1101" 13
        parseInteger "0b0" 0
      it "does not parse numbers prefixed with `0B`" $ do
        parseInteger "0B1101" 0
      it "can parse numbers with leading zeros after the prefix" $ do
        parseInteger "0b000" 0
        parseInteger "0b00011" 3
      it "does not parse negative numbers" $ do
        parseInteger "-0b101" 0
      it "does not parse numbers with non-valid binary digits" $ do
        parseInteger "0b123" 1
    context "when the integer is in octal representation" $ do
      it "can parse numbers prefixed with `0o`" $ do
        parseInteger "0o567" 0o567
        parseInteger "0o0" 0
      it "does not parse numbers prefixed with `0O`" $ do
        parseInteger "0O567" 0
      it "can parse numbers with leading zeros after the prefix" $ do
        parseInteger "0o000000" 0
        parseInteger "0o000567" 0o567
      it "does not parse negative numbers" $ do
        parseInteger "-0o123" 0
      it "does not parse numbers with non-valid octal digits" $ do
        parseInteger "0o789" 0o7
    context "when the integer is in hexadecimal representation" $ do
      it "can parse numbers prefixed with `0x`" $ do
        parseInteger "0x12af" 0x12af
        parseInteger "0x0" 0
      it "does not parse numbers prefixed with `0X`" $ do
        parseInteger "0Xfff" 0
      it "can parse numbers with leading zeros after the prefix" $ do
        parseInteger "0x00000" 0
        parseInteger "0x012af" 0x12af
      it "does not parse negative numbers" $ do
        parseInteger "-0xfff" 0
      it "does not parse numbers with non-valid hexadecimal digits" $ do
        parseInteger "0xfgh" 0xf
      it "can parse numbers when hex digits are lowercase" $ do
        parseInteger "0xabcdef" 0xabcdef
      it "can parse numbers when hex digits are uppercase" $ do
        parseInteger "0xABCDEF" 0xABCDEF
      it "can parse numbers when hex digits are in both lowercase and uppercase" $ do
        parseInteger "0xAbCdEf" 0xAbCdEf
        parseInteger "0xaBcDeF" 0xaBcDeF

  describe "keyP" $ do
    context "when the key is a bare key" $ do
      it "can parse keys which contain ASCII letters, digits, underscores, and dashes" $ do
        parseKey "key"       (makeKey ["key"])
        parseKey "bare_key1" (makeKey ["bare_key1"])
        parseKey "bare-key2" (makeKey ["bare-key2"])
      it "can parse keys which contain only digits" $ do
        parseKey "1234" (makeKey ["1234"])
    context "when the key is a quoted key" $ do
      it "can parse keys that follow the exact same rules as basic strings" $ do
        parseKey (dquote "127.0.0.1")          (makeKey [dquote "127.0.0.1"])
        parseKey (dquote "character encoding") (makeKey [dquote "character encoding"])
        parseKey (dquote "ʎǝʞ")                (makeKey [dquote "ʎǝʞ"])
      it "can parse keys that follow the exact same rules as literal strings" $ do
        parseKey (squote "key2")             (makeKey [squote "key2"])
        parseKey (squote "quoted \"value\"") (makeKey [squote "quoted \"value\""])
    context "when the key is a dotted key" $ do
      it "can parse a sequence of bare or quoted keys joined with a dot" $ do
        parseKey "name"                (makeKey ["name"])
        parseKey "physical.color"      (makeKey ["physical", "color"])
        parseKey "physical.shape"      (makeKey ["physical", "shape"])
        parseKey "site.\"google.com\"" (makeKey ["site", dquote "google.com"])
      --xit "ignores whitespaces around dot-separated parts" $ do
      --  parseKey "a . b . c. d" (makeKey ["a", "b", "c", "d"])

  describe "keyValP" $ do
    it "can parse key/value pairs" $ do
      parseKeyVal "x='abcdef'"  (makeKey ["x"], AnyValue (Text "abcdef"))
      parseKeyVal "x=1"         (makeKey ["x"], AnyValue (Integer 1))
      parseKeyVal "x=5.2"       (makeKey ["x"], AnyValue (Double 5.2))
      parseKeyVal "x=true"      (makeKey ["x"], AnyValue (Bool True))
      parseKeyVal "x=[1, 2, 3]" (makeKey ["x"], AnyValue (Array [Integer 1, Integer 2, Integer 3]))
    --xit "can parse a key/value pair when the value is a date" $ do
    --  let makeDay y m d = (AnyValue .Date . Day) (fromGregorian y m d)

    --  parseKeyVal "x = 1920-12-10" (makeKey ["x"], makeDay 1920 12 10)
    --xit "can parse a key/value pair when the value is an inline table" $ do
    --  pending
    it "ignores white spaces around key names and values" $ do
      parseKeyVal "x=1    "   (makeKey ["x"], AnyValue (Integer 1))
      parseKeyVal "x=    1"   (makeKey ["x"], AnyValue (Integer 1))
      parseKeyVal "x    =1"   (makeKey ["x"], AnyValue (Integer 1))
      parseKeyVal "x\t= 1 "   (makeKey ["x"], AnyValue (Integer 1))
      parseKeyVal "\"x\" = 1" (makeKey [dquote "x"], AnyValue (Integer 1))
    --xit "fails if the key, equals sign, and value are not on the same line" $ do
    --  keyValFailOn "x\n=\n1"
    --  keyValFailOn "x=\n1"
    --  keyValFailOn "\"x\"\n=\n1"
    it "works if the value is broken over multiple lines" $ do
      parseKeyVal "x=[1, \n2\n]" (makeKey ["x"], AnyValue (Array [Integer 1, Integer 2]))
    it "fails if the value is not specified" $ do
      keyValFailOn "x="

  describe "textP" $ do
    context "when the string is a basic string" $ do
      it "can parse strings surrounded by double quotes" $ do
        parseText (dquote "xyz") "xyz"
        parseText (dquote "")    ""
        textFailOn "\"xyz"
        textFailOn "xyz\""
        textFailOn "xyz"
      it "can parse escaped quotation marks, backslashes, and control characters" $ do
       parseText (dquote "backspace: \\b")               "backspace: \b"
       parseText (dquote "tab: \\t")                     "tab: \t"
       parseText (dquote "linefeed: \\n")                "linefeed: \n"
       parseText (dquote "form feed: \\f")               "form feed: \f"
       parseText (dquote "carriage return: \\r")         "carriage return: \r"
       parseText (dquote "quote: \\\"")                  "quote: \""
       parseText (dquote "backslash: \\\\")              "backslash: \\"
       parseText (dquote "a\\uD7FFxy\\U0010FFFF\\uE000") "a\55295xy\1114111\57344"
      it "fails if the string has an unescaped backslash, or control character" $ do
       textFailOn (dquote "new \n line")
       textFailOn (dquote "back \\ slash")
      it "fails if the string has an escape sequence that is not listed in the TOML specification" $ do
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
      let dquote3 = quoteWith "\"\"\""

      it "can parse multi-line strings surrounded by three double quotes" $ do
        parseText (dquote3 "Roses are red\nViolets are blue") "Roses are red\nViolets are blue"
      it "can parse single-line strings surrounded by three double quotes" $ do
        parseText (dquote3 "Roses are red Violets are blue") "Roses are red Violets are blue"
      it "can parse all of the escape sequences that are valid for basic strings" $ do
        parseText (dquote3 "backspace: \\b")               "backspace: \b"
        parseText (dquote3 "tab: \\t")                     "tab: \t"
        parseText (dquote3 "linefeed: \\n")                "linefeed: \n"
        parseText (dquote3 "form feed: \\f")               "form feed: \f"
        parseText (dquote3 "carriage return: \\r")         "carriage return: \r"
        parseText (dquote3 "quote: \\\"")                  "quote: \""
        parseText (dquote3 "backslash: \\\\")              "backslash: \\"
        parseText (dquote3 "a\\uD7FFxy\\U0010FFFF\\uE000") "a\55295xy\1114111\57344"
      it "does not ignore whitespaces or newlines" $ do
        parseText (dquote3 "\nabc  \n   xyz") "abc  \n   xyz"
      it "ignores a newline only if it immediately follows the opening delimiter" $ do
        parseText (dquote3 "\nThe quick brown") "The quick brown"
      it "ignores whitespaces and newlines after line ending backslash" $ do
        parseText (dquote3 "The quick brown \\\n\n   fox jumps over") "The quick brown fox jumps over"
      it "fails if the string has an unescaped backslash, or control character" $ do
        textFailOn (dquote3 "backslash \\ .")
        textFailOn (dquote3 "backspace \b ..")
        textFailOn (dquote3 "tab \t ..")
    context "when the string is a literal string" $ do
      it "can parse strings surrounded by single quotes" $ do
        parseText (squote "C:\\Users\\nodejs\\templates")    "C:\\Users\\nodejs\\templates"
        parseText (squote "\\\\ServerX\\admin$\\system32\\") "\\\\ServerX\\admin$\\system32\\"
        parseText (squote "Tom \"Dubs\" Preston-Werner")     "Tom \"Dubs\" Preston-Werner"
        parseText (squote "<\\i\\c*\\s*>")                   "<\\i\\c*\\s*>"
        parseText (squote "a \t tab")                        "a \t tab"
      it "fails if the string is not on a single line" $ do
        textFailOn (squote "\nabc")
        textFailOn (squote "ab\r\nc")
        textFailOn (squote "abc\n")
    context "when the string is a multi-line literal string" $ do
      let squote3 = quoteWith "'''"

      it "can parse multi-line strings surrounded by three single quotes" $ do
        parseText (squote3 "first line \nsecond.\n   3\n") "first line \nsecond.\n   3\n"
      it "can parse single-line strings surrounded by three single quotes" $ do
        parseText (squote3 "I [dw]on't need \\d{2} apples") "I [dw]on't need \\d{2} apples"
      it "ignores a newline immediately following the opening delimiter" $ do
        parseText (squote3 "\na newline \nsecond.\n   3\n") "a newline \nsecond.\n   3\n"
      it "fails if the string has an unescaped control character other than tab" $ do
        parseText (squote3 "\t") "\t"
        textFailOn (squote3 "\b")

  describe "tableHeaderP" $ do
    it "can parse a TOML table" $ do
      let key1KV = (makeKey ["key1"], AnyValue (Text "some string"))
          key2KV = (makeKey ["key2"], AnyValue (Integer 123))
          table  = (makeKey ["table-1"], tomlFromList [key1KV, key2KV])

      parseTable "[table-1]\nkey1 = \"some string\"\nkey2 = 123" table
    it "can parse an empty TOML table" $ do
      parseTable "[table]" (makeKey ["table"], tomlFromList [])
    it "allows the name of the table to be any valid TOML key" $ do
      parseTable "[dog.\"tater.man\"]" (makeKey ["dog", dquote "tater.man"], tomlFromList [])
      parseTable "[j.\"ʞ\".'l']"       (makeKey ["j", dquote "ʞ", squote "l"], tomlFromList [])

  describe "tomlP" $ do
    it "can parse TOML files" $ do
      let tomlString = " # This is a TOML document.\n\n \
                       \ title = \"TOML Example\" # Comment \n\n \
                       \ [owner]\n \
                       \ name = \"Tom Preston-Werner\" \
                       \ enabled = true # First class dates"

          titleKV    = (makeKey ["title"], AnyValue (Text "TOML Example"))
          nameKV     = (makeKey ["name"], AnyValue (Text "Tom Preston-Werner"))
          enabledKV  = (makeKey ["enabled"], AnyValue (Bool True))
          tomlPairs  = HashMap.fromList [titleKV]
          tomlTables = fromList [(makeKey ["owner"], tomlFromList [nameKV, enabledKV])]

      parseToml tomlString (TOML tomlPairs tomlTables)
