module Test.Toml.Parser.Text
    ( textSpecs
    ) where

import Test.Hspec (Spec, context, describe, it)

import Test.Toml.Parser.Common (dquote, dquote3, parseText, squote, squote3, textFailOn,
                                tomlFailOn)


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
        it "can parse the escape sequences added in TOML 1.1.0" $ do
            parseText (dquote "escape: \\e")   "escape: \ESC"
            parseText (dquote "hex: \\x41\\x7a") "hex: Az"
            parseText (dquote "S\\xf8rmirb\\xE6ren") "S\248rmirb\230ren"
            parseText (dquote "nul: \\x00")     "nul: \0"
            textFailOn (dquote "\\x4")
            textFailOn (dquote "\\xg1")
        it "can parse tabs and non-ASCII characters" $ do
            parseText (dquote "a\tb") "a\tb"
            parseText (dquote "\128 \255 \55295 \57344 \65535 \65536 \1114111")
                      "\128 \255 \55295 \57344 \65535 \65536 \1114111"
        it "fails if the string has an unescaped backslash, or control character" $ do
            textFailOn (dquote "new \n line")
            textFailOn (dquote "back \\ slash")
            textFailOn (dquote "nul \0")
            textFailOn (dquote "del \DEL")
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
        it "ignores whitespaces and newlines after line ending backslash" $ do
            parseText (dquote3 "The quick brown \\\n\n   fox jumps over") "The quick brown fox jumps over"
            parseText (dquote3 "The quick brown \\  \t\n\n   fox jumps over") "The quick brown fox jumps over"
            parseText (dquote3 "\\\n   \\\n   \\  \n   ") ""
        it "allows one or two quotation marks next to the delimiters" $ do
            parseText (dquote3 "\"one quote\"") "\"one quote\""
            parseText (dquote3 "\"\"two quotes\"\"") "\"\"two quotes\"\""
            parseText (dquote3 "lol\\\"\"\"") "lol\"\"\""
            parseText (dquote3 "a \"\" b") "a \"\" b"
            tomlFailOn ("x = " <> dquote3 "\"\"\"three quotes\"\"\"")
        it "fails if the string has an unescaped backslash, or control character other than tab" $ do
            parseText  (dquote3 "tab \t ..") "tab \t .."
            textFailOn (dquote3 "backslash \\ .")
            textFailOn (dquote3 "backspace \b ..")
            textFailOn (dquote3 "bare cr \r ..")
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
        it "does not interpret escape sequences" $
            parseText (squote "\\x20 \\e \\u0041") "\\x20 \\e \\u0041"
        it "fails if the string is not on a single line" $ do
            textFailOn (squote "\nabc")
            textFailOn (squote "ab\r\nc")
            textFailOn (squote "abc\n")
        it "fails if the string has a control character other than tab" $ do
            textFailOn (squote "nul \0")
            textFailOn (squote "del \DEL")
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
        it "allows one or two apostrophes next to the delimiters" $ do
            parseText (squote3 "'one quote'") "'one quote'"
            parseText (squote3 "''two quotes''") "''two quotes''"
            parseText (squote3 " 'one quote' ") " 'one quote' "
            tomlFailOn ("x = " <> squote3 "'''three quotes'''")
        it "fails if the string has an unescaped control character other than tab" $ do
            parseText (squote3 "\t") "\t"
            textFailOn (squote3 "\b")
            textFailOn (squote3 "bare cr \r ..")
