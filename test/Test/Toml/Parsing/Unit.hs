{-# LANGUAGE FlexibleContexts #-}

module Test.Toml.Parsing.Unit where

import Data.Text (singleton)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Test.Tasty.Hspec (Spec, context, describe, it)
import Text.Megaparsec (parse)

import Toml.Parser (arrayP, boolP, integerP)
import Toml.Type (UValue (..))

spec_Parser :: Spec
spec_Parser = do
  let parseX p given expected = parse p "" given `shouldParse` expected
      failOn p given          = parse p "" `shouldFailOn` given

  let parseArray  = parseX arrayP
      parseBool   = parseX boolP
      parseInt    = parseX integerP
      arrayFailOn = failOn arrayP
      boolFailOn  = failOn boolP

  describe "arrayP" $ do
    it "can parse arrays" $ do
      parseArray "[]" []
      parseArray "[1]" [UInt 1]
      parseArray "[1, 2, 3]" [UInt 1, UInt 2, UInt 3]
      parseArray "[1.2, 2.3, 3.4]" [UFloat 1.2, UFloat 2.3, UFloat 3.4]
      parseArray "['x', 'y']" [UString (singleton 'x'), UString (singleton 'y')]
      parseArray "[[1], [2]]" [UArray [UInt 1], UArray [UInt 2]]
      -- TODO: Add a test for array of UDate after implementing issue #18
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
    it "fails if the array has elements of different types" $ do
      arrayFailOn "[1, 2.4]"
      arrayFailOn "[1, 'y', 2]"
      arrayFailOn "[[2, 3], 'x']"
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
      -- TODO: After implementing issue #64
      --it "does not parse numbers with leading zeros" $ do
      --  parseInt "0123" 0
      --  parseInt "-023" 0
      -- TODO: After implementing issue #17
      --it "can parse numbers with underscores between digits" $ do
      --  parseInt "1_000" 1000
      --  parseInt "5_349_221" 5349221
      --  parseInt "1_2_3_4_5" 12345
      --  parseInt "1_2_3_" 1
      --  parseInt "13_" 13
      --  intFailOn "_123_"
      --  intFailOn "_13"
      --  intFailOn "_"
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
