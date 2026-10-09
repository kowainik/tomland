module Test.Toml.Parser.Array
    ( arraySpecs
    ) where

import Data.Time (TimeOfDay (..))
import Test.Hspec (Spec, describe, it)

import Data.Time (LocalTime (..))

import Test.Toml.Parser.Common (arrayFailOn, day1, day2, hours1, int1, int2, int3, int4,
                                makeZoned, offset0, parseArray)
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
    it "can parse arrays with elements of different types" $ do
        parseArray
            "[1, 1.5, 'x', true]"
            [int1, UDouble 1.5, UText "x", UBool True]
        parseArray
            "[1979-05-27T07:32:00Z, 1979-05-27T07:32:00, 1979-05-27, 07:32:00]"
            [ makeZoned day1 hours1 offset0
            , ULocal (LocalTime day1 hours1)
            , UDay day1
            , UHours hours1
            ]
        parseArray
            "[1, [2], [[3]]]"
            [int1, UArray [int2], UArray [UArray [int3]]]
    it "can parse inline tables inside arrays" $ do
        parseArray
            "[{a = 1}, 'x']"
            [UTable [("a", int1)], UText "x"]
        parseArray
            "[[{}]]"
            [UArray [UTable []]]
        parseArray
            "[ { a = { b = 1 }, c = [ { d = 2 } ] } ]"
            [UTable [("a", UTable [("b", int1)]), ("c", UArray [UTable [("d", int2)]])]]
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
            "[1, 2,\n]"
            [int1, int2]
    it "fails on more than one terminating comma or on consecutive commas" $ do
        arrayFailOn "[1, 2, 3, , ,]"
        arrayFailOn "[1,,2]"
        arrayFailOn "[,]"
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
