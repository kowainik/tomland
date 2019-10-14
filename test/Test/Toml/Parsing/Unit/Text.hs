{-# LANGUAGE FlexibleContexts #-}

module Test.Toml.Parsing.Unit.Text where

import Test.Tasty.Hspec (Spec, context, describe, it)

import Test.Toml.Parsing.Unit.Common (dquote, dquote3, parseText, squote, squote3, textFailOn)

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
