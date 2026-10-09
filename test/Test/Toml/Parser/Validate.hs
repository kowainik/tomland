module Test.Toml.Parser.Validate
    ( validateSpec
    ) where

import Hedgehog (evalEither, forAll)
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)

import Test.Toml.Gen (genToml)
import Toml.Parser.Item (TomlItem (..), tomlP)
import Toml.Parser.Validate (ValidationError (..), validateItems)
import Toml.Type.Key (Key)
import Toml.Type.Printer (pretty)
import Toml.Type.UValue (UValue (..))


validateSpec :: Spec
validateSpec = describe "Parser Validation tests" $ do
  -- property success
    validationProperty
    -- failure
    validationFail
        [keyVal "key", keyVal "key"]
        (DuplicateKey "key")
    validationFail
        [TableName "table", TableName "table"]
        (DuplicateTable "table")
    validationFail
        [keyVal "keyAndTable", TableName "keyAndTable"]
        (SameNameKeyTable "keyAndTable")
    validationFail
        [TableName "tableArray", TableArrayName "tableArray"]
        (SameNameTableArray "tableArray")
    validationFail
        [TableArrayName "tableArray", TableName "tableArray"]
        (SameNameTableArray "tableArray")
    validationFail
        [inlineTable "inline", TableName "inline"]
        (DuplicateTable "inline")
    validationFail
        [keyVal "inline", inlineTable "inline"]
        (SameNameKeyTable "inline")
    validationFail
        [inlineTableArray, TableName "inlinearray"]
        (SameNameKeyTable "inlinearray")
    validationFail
        [inlineTableArray, inlineTable "inlinearray"]
        (SameNameKeyTable "inlinearray")
    validationFail
        [inlineTable "inlinearray", inlineTableArray]
        (SameNameKeyTable "inlinearray")
    validationFail
        [KeyVal "arr" (UBool True), TableArrayName "arr"]
        (SameNameKeyTable "arr")
    validationOk
        [TableArrayName "arr", KeyVal "arr" (UBool True)]
    validationFail
        [TableName "a", KeyVal "x" (UBool True), TableName "a"]
        (DuplicateTable "a")
    validationFail
        [TableName "a.b", TableName "a", TableName "a.b"]
        (DuplicateTable "a.b")
    validationFail
        [KeyVal "a" (UBool True), KeyVal "a.b" (UBool True)]
        (SameNameKeyTable "a")
    validationFail
        [KeyVal "a.b" (UBool True), KeyVal "a" (UBool True)]
        (SameNameKeyTable "a")
    validationFail
        [KeyVal "a.b" (UBool True), TableName "a"]
        (DuplicateTable "a")
    validationFail
        [inlineTable "a", KeyVal "a.b" (UBool True)]
        (ExtendClosedTable "a")
    validationFail
        [inlineTable "a", TableName "a.b"]
        (ExtendClosedTable "a")
    validationFail
        [TableName "a.b", TableName "a", KeyVal "b.c" (UBool True)]
        (ExtendClosedTable "a.b")
    validationFail
        [TableArrayName "a.b", TableName "a", KeyVal "b.c" (UBool True)]
        (SameNameTableArray "a.b")
    validationFail
        [KeyVal "t" (UTable [("a", UBool True), ("a", UBool False)])]
        (DuplicateKey "t.a")
    validationFail
        [KeyVal "key" (UBool True), KeyVal "\"key\"" (UBool True)]
        (DuplicateKey "key")
    validationOk
        [TableName "a.b", TableName "a", TableName "a.c"]
    validationOk
        [TableName "a", KeyVal "b.c" (UBool True), TableName "a.b.d"]
    validationOk
        [KeyVal "a.b" (UBool True), TableName "a.c"]
    validationOk
        [TableArrayName "a", TableName "a.b", TableArrayName "a", TableName "a.b"]
    validationOk
        [KeyVal "t" (UTable [("a.b", UBool True), ("a.c", UBool False)])]

  where
    keyVal :: Key -> TomlItem
    keyVal k = KeyVal k (UBool True)

    inlineTable :: Key -> TomlItem
    inlineTable k = KeyVal k (UTable [])

    inlineTableArray :: TomlItem
    inlineTableArray = KeyVal "inlinearray" (UArray [UTable []])

validationProperty :: SpecWith (Arg Expectation)
validationProperty = it "Property: validates any generated TOML" $ hedgehog $ do
    toml <- forAll genToml
    let tomlText = pretty toml
    tomlItems <- evalEither $ parse tomlP "" tomlText
    _ <- evalEither (validateItems tomlItems)
    pure ()

validationOk :: [TomlItem] -> SpecWith (Arg Expectation)
validationOk items = it ("accepts: " ++ show items) $
    either (const False) (const True) (validateItems items) `shouldBe` True

validationFail :: [TomlItem] -> ValidationError -> SpecWith (Arg Expectation)
validationFail tomlItems validationError = it ("fail on " ++ show validationError) $
    validateItems tomlItems `shouldBe` Left validationError
