module Test.Toml.Parser.Array
    ( arraySpecs
    ) where

import Data.Time (TimeOfDay (..))
import Test.Hspec (Spec, describe, it)

import Test.Toml.Parser.Common (arrayFailOn, day1, day2, int1, int2, int3, int4, parseArray)
import Toml.Type (UValue (..))


arraySpecs :: Spec
arraySpecs = describe "arrayP" $ do
    it "can parse arrays" $ do
        parseArray
            "[]"
            []
        parseArray
            "[1]"
            [int1]
        parseArray
            "[1, 2, 3]"
            [int1, int2, int3]
        parseArray
            "[1.2, 2.3, 3.4]"
            [UDouble 1.2, UDouble 2.3, UDouble 3.4]
        parseArray
            "['x', 'y']"
            [UText "x", UText "y"]
        parseArray
            "[[1], [2]]"
            [UArray [UInteger 1], UArray [UInteger 2]]
        parseArray
            "[1920-12-10, 1979-05-27]"
            [UDay  day2, UDay day1]
        parseArray
            "[16:33:05, 10:15:30]"
            [UHours (TimeOfDay 16 33 5), UHours (TimeOfDay 10 15 30)]
    it "can parse multiline arrays" $
        parseArray
            "[\n1,\n2\n]"
            [int1, int2]
    it "can parse an array of arrays" $
        parseArray
            "[[1], [2.3, 5.1]]"
            [UArray [int1], UArray [UDouble 2.3, UDouble 5.1]]
    it "can parse an array with terminating commas (trailing commas)" $ do
        parseArray
            "[1, 2,]"
            [int1, int2]
        parseArray
            "[1, 2, 3, , ,]"
            [int1, int2, int3]
    it "allows an arbitrary number of comments and newlines before or after a value" $
        parseArray
            "[\n\n#c\n1, #c 2 \n 2, \n\n\n 3, #c \n #c \n 4]"
            [int1, int2, int3, int4]
    it "ignores white spaces" $
        parseArray
            "[   1    ,    2,3,  4      ]"
            [int1, int2, int3, int4]

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
