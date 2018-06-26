{-# LANGUAGE FlexibleContexts #-}

module Test.Toml.Parsing.Unit where

import Data.Semigroup ((<>))
import Data.Time (TimeOfDay (..), fromGregorian)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Test.Tasty.Hspec (Spec, context, describe, it, pending, xit)
import Text.Megaparsec (parse)

import Toml.Parser (arrayP, boolP, floatP, integerP, keyP, keyValP, stringP, tableHeaderP, tomlP)
import Toml.PrefixTree (Key (..), Piece (..), fromList)
import Toml.Type (AnyValue (..), DateTime (..), TOML (..), UValue (..), Value (..))

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List.NonEmpty as NE

spec_Parser :: Spec
spec_Parser = do
  let parseX p given expected = parse p "" given `shouldParse` expected
      failOn p given          = parse p "" `shouldFailOn` given

      parseArray   = parseX arrayP
      parseBool    = parseX boolP
      parseFloat   = parseX floatP
      parseInt     = parseX integerP
      parseKey     = parseX keyP
      parseKeyVal  = parseX keyValP
      parseString  = parseX stringP
      parseTable   = parseX tableHeaderP
      parseToml    = parseX tomlP
      arrayFailOn  = failOn arrayP
      boolFailOn   = failOn boolP
      intFailOn    = failOn integerP
      keyValFailOn = failOn keyValP
      stringFailOn = failOn stringP

      quoteWith q t = q <> t <> q
      squote        = quoteWith "'"
      dquote        = quoteWith "\""

      makeKey k = (Key . NE.fromList) (map Piece k)

      tomlFromList kv = TOML (HashMap.fromList kv) mempty


  describe "arrayP" $ do
    it "can parse arrays" $ do
      parseArray "[]" []
      parseArray "[1]" [UInt 1]
      parseArray "[1, 2, 3]" [UInt 1, UInt 2, UInt 3]
      parseArray "[1.2, 2.3, 3.4]" [UFloat 1.2, UFloat 2.3, UFloat 3.4]
      parseArray "['x', 'y']" [UString "x", UString "y"]
      parseArray "[[1], [2]]" [UArray [UInt 1], UArray [UInt 2]]
    xit "can parse arrays of dates" $ do
      let makeDay   y m d = (UDate . Day) (fromGregorian y m d)
          makeHours h m s = (UDate . Hours) (TimeOfDay h m s)
      parseArray "[1920-12-10, 10:15:30]" [makeDay 1920 12 10, makeHours 10 15 30]
    it "can parse multiline arrays" $ do
      parseArray "[\n1,\n2\n]" [UInt 1, UInt 2]
    it "can parse an array of arrays" $ do
      parseArray "[[1], [2.3, 5.1]]" [UArray [UInt 1], UArray [UFloat 2.3, UFloat 5.1]]
    it "can parse an array with terminating commas (trailing commas)" $ do
      parseArray "[1, 2,]" [UInt 1, UInt 2]
      parseArray "[1, 2, 3, , ,]" [UInt 1, UInt 2, UInt 3]
    it "allows an arbitrary number of comments and newlines before or after a value" $ do
      parseArray "[\n\n#c\n1, #c 2 \n 2, \n\n\n 3, #c \n #c \n 4]" [UInt 1, UInt 2, UInt 3, UInt 4]
    it "ignores white spaces" $ do
      parseArray "[   1    ,    2,3,  4      ]" [UInt 1, UInt 2, UInt 3, UInt 4]
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

  describe "floatP" $ do
      it "can parse a number which consists of an integral part, and a fractional part" $ do
        parseFloat "+1.0" 1.0
        parseFloat "3.1415" 3.1415
        parseFloat "0.0" 0.0
        parseFloat "-0.01" (-0.01)
      it "can parse a number which consists of an integral part, and an exponent part" $ do
        parseFloat "5e+22" 5e+22
        parseFloat "1e6" 1e6
        parseFloat "-2E-2" (-2E-2)
      it "can parse a number which consists of an integral, a fractional, and an exponent part" $ do
        parseFloat "6.626e-34" 6.626e-34
      it "can parse sign-prefixed zero" $ do
        parseFloat "+0.0" 0.0
        parseFloat "-0.0" (-0.0)

  describe "integerP" $ do
    context "when the integer is in decimal representation" $ do
      it "can parse positive integer numbers" $ do
        parseInt "10" 10
        parseInt "+3" 3
        parseInt "0" 0
      it "can parse negative integer numbers" $ do
        parseInt "-123" (-123)
      it "can parse sign-prefixed zero as an unprefixed zero" $ do
        parseInt "+0" 0
        parseInt "-0" 0
      it "can parse both the minimum and maximum numbers in the 64 bit range" $ do
        parseInt "-9223372036854775808" (-9223372036854775808)
        parseInt "9223372036854775807" 9223372036854775807
      xit "can parse numbers with underscores between digits" $ do
        parseInt "1_000" 1000
        parseInt "5_349_221" 5349221
        parseInt "1_2_3_4_5" 12345
        parseInt "1_2_3_" 1
        parseInt "13_" 13
        intFailOn "_123_"
        intFailOn "_13"
        intFailOn "_"
      xit "does not parse numbers with leading zeros" $ do
        parseInt "0123" 0
        parseInt "-023" 0
    context "when the integer is in binary representation" $ do
      it "can parse numbers prefixed with `0b`" $ do
        parseInt "0b1101" 13
        parseInt "0b0" 0
      it "does not parse numbers prefixed with `0B`" $ do
        parseInt "0B1101" 0
      it "can parse numbers with leading zeros after the prefix" $ do
        parseInt "0b000" 0
        parseInt "0b00011" 3
      it "does not parse negative numbers" $ do
        parseInt "-0b101" 0
      it "does not parse numbers with non-valid binary digits" $ do
        parseInt "0b123" 1
    context "when the integer is in octal representation" $ do
      it "can parse numbers prefixed with `0o`" $ do
        parseInt "0o567" 0o567
        parseInt "0o0" 0
      it "does not parse numbers prefixed with `0O`" $ do
        parseInt "0O567" 0
      it "can parse numbers with leading zeros after the prefix" $ do
        parseInt "0o000000" 0
        parseInt "0o000567" 0o567
      it "does not parse negative numbers" $ do
        parseInt "-0o123" 0
      it "does not parse numbers with non-valid octal digits" $ do
        parseInt "0o789" 0o7
    context "when the integer is in hexadecimal representation" $ do
      it "can parse numbers prefixed with `0x`" $ do
        parseInt "0x12af" 0x12af
        parseInt "0x0" 0
      it "does not parse numbers prefixed with `0X`" $ do
        parseInt "0Xfff" 0
      it "can parse numbers with leading zeros after the prefix" $ do
        parseInt "0x00000" 0
        parseInt "0x012af" 0x12af
      it "does not parse negative numbers" $ do
        parseInt "-0xfff" 0
      it "does not parse numbers with non-valid hexadecimal digits" $ do
        parseInt "0xfgh" 0xf
      it "can parse numbers when hex digits are lowercase" $ do
        parseInt "0xabcdef" 0xabcdef
      it "can parse numbers when hex digits are uppercase" $ do
        parseInt "0xABCDEF" 0xABCDEF
      it "can parse numbers when hex digits are in both lowercase and uppercase" $ do
        parseInt "0xAbCdEf" 0xAbCdEf
        parseInt "0xaBcDeF" 0xaBcDeF

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
      xit "ignores whitespaces around dot-separated parts" $ do
        parseKey "a . b . c. d" (makeKey ["a", "b", "c", "d"])

  describe "keyValP" $ do
    it "can parse key/value pairs" $ do
      parseKeyVal "x='abcdef'"  (makeKey ["x"], AnyValue (String "abcdef"))
      parseKeyVal "x=1"         (makeKey ["x"], AnyValue (Int 1))
      parseKeyVal "x=5.2"       (makeKey ["x"], AnyValue (Float 5.2))
      parseKeyVal "x=true"      (makeKey ["x"], AnyValue (Bool True))
      parseKeyVal "x=[1, 2, 3]" (makeKey ["x"], AnyValue (Array [Int 1, Int 2, Int 3]))
    xit "can parse a key/value pair when the value is a date" $ do
      let makeDay y m d = (AnyValue .Date . Day) (fromGregorian y m d)

      parseKeyVal "x = 1920-12-10" (makeKey ["x"], makeDay 1920 12 10)
    xit "can parse a key/value pair when the value is an inline table" $ do
      pending
    it "ignores white spaces around key names and values" $ do
      parseKeyVal "x=1    "   (makeKey ["x"], AnyValue (Int 1))
      parseKeyVal "x=    1"   (makeKey ["x"], AnyValue (Int 1))
      parseKeyVal "x    =1"   (makeKey ["x"], AnyValue (Int 1))
      parseKeyVal "x\t= 1 "   (makeKey ["x"], AnyValue (Int 1))
      parseKeyVal "\"x\" = 1" (makeKey [dquote "x"], AnyValue (Int 1))
    xit "fails if the key, equals sign, and value are not on the same line" $ do
      keyValFailOn "x\n=\n1"
      keyValFailOn "x=\n1"
      keyValFailOn "\"x\"\n=\n1"
    it "works if the value is broken over multiple lines" $ do
      parseKeyVal "x=[1, \n2\n]" (makeKey ["x"], AnyValue (Array [Int 1, Int 2]))
    it "fails if the value is not specified" $ do
      keyValFailOn "x="

  describe "stringP" $ do
    context "when the string is a basic string" $ do
      it "can parse strings surrounded by double quotes" $ do
        parseString (dquote "xyz") "xyz"
        parseString (dquote "")    ""
        stringFailOn "\"xyz"
        stringFailOn "xyz\""
        stringFailOn "xyz"
      xit "can parse escaped quotation marks, backslashes, and control characters" $ do
        parseString (dquote "backspace: \\b")               "backspace: \b"
        parseString (dquote "tab: \\t")                     "tab: \t"
        parseString (dquote "linefeed: \\n")                "linefeed: \n"
        parseString (dquote "form feed: \\f")               "form feed: \f"
        parseString (dquote "carriage return: \\r")         "carriage return: \r"
        parseString (dquote "quote: \\\"")                  "quote: \""
        parseString (dquote "backslash: \\\\")              "backslash: \\"
        parseString (dquote "a\\uD7FFxy\\U0010FFFF\\uE000") "a\55295xy\1114111\57344"
      xit "fails if the string has an unescaped backslash, or control character" $ do
        stringFailOn (dquote "new \n line")
        stringFailOn (dquote "back \\ slash")
      xit "fails if the string has an escape sequence that is not listed in the TOML specification" $ do
        stringFailOn (dquote "xy\\z \\abc")
      xit "fails if the string is not on a single line" $ do
        stringFailOn (dquote "\nabc")
        stringFailOn (dquote "ab\r\nc")
        stringFailOn (dquote "abc\n")
      xit "fails if escape codes are not valid Unicode scalar values" $ do
        stringFailOn (dquote "\\u1")
        stringFailOn (dquote "\\uxyzw")
        stringFailOn (dquote "\\U0000")
        stringFailOn (dquote "\\uD8FF")
        stringFailOn (dquote "\\U001FFFFF")
    context "when the string is a literal string" $ do
      it "can parse strings surrounded by single quotes" $ do
        parseString (squote "C:\\Users\\nodejs\\templates")    "C:\\Users\\nodejs\\templates"
        parseString (squote "\\\\ServerX\\admin$\\system32\\") "\\\\ServerX\\admin$\\system32\\"
        parseString (squote "Tom \"Dubs\" Preston-Werner")     "Tom \"Dubs\" Preston-Werner"
        parseString (squote "<\\i\\c*\\s*>")                   "<\\i\\c*\\s*>"
        parseString (squote "a \t tab")                        "a \t tab"
      xit "fails if the string is not on a single line" $ do
        stringFailOn (squote "\nabc")
        stringFailOn (squote "ab\r\nc")
        stringFailOn (squote "abc\n")

  describe "tableHeaderP" $ do
    it "can parse a TOML table" $ do
      let key1KV = (makeKey ["key1"], AnyValue (String "some string"))
          key2KV = (makeKey ["key2"], AnyValue (Int 123))
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

          titleKV    = (makeKey ["title"], AnyValue (String "TOML Example"))
          nameKV     = (makeKey ["name"], AnyValue (String "Tom Preston-Werner"))
          enabledKV  = (makeKey ["enabled"], AnyValue (Bool True))
          tomlPairs  = HashMap.fromList [titleKV]
          tomlTables = fromList [(makeKey ["owner"], tomlFromList [nameKV, enabledKV])]

      parseToml tomlString (TOML tomlPairs tomlTables)
