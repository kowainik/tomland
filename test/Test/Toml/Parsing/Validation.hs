module Test.Toml.Parsing.Validation
    ( parsingValidationSpec
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Hedgehog (evalEither, forAll)
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)

import Test.Toml.Gen (genToml)
import Toml.Parser.Item (Table (..), TomlItem (..), tomlP)
import Toml.Parser.Validate (ValidationError (..), validateItems)
import Toml.PrefixTree (Key)
import Toml.Printer (pretty)
import Toml.Type (AnyValue (..), Value (..))


parsingValidationSpec :: Spec
parsingValidationSpec = describe "Parser Validation tests" $ do
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
        (SameNameTableArray "inlinearray")
    validationFail
        [inlineTableArray, inlineTable "inlinearray"]
        (SameNameTableArray "inlinearray")
    validationFail
        [inlineTable "inlinearray", inlineTableArray]
        (SameNameTableArray "inlinearray")

  where
    keyVal :: Key -> TomlItem
    keyVal k = KeyVal k (AnyValue $ Bool True)

    inlineTable :: Key -> TomlItem
    inlineTable k = InlineTable k table

    inlineTableArray :: TomlItem
    inlineTableArray = InlineTableArray "inlinearray" (table :| [])

    table :: Table
    table = Table []

validationProperty :: SpecWith (Arg Expectation)
validationProperty = it "Property: validates any generated TOML" $ hedgehog $ do
    toml <- forAll genToml
    let tomlText = pretty toml
    tomlItems <- evalEither $ parse tomlP "" tomlText
    _ <- evalEither (validateItems tomlItems)
    pure ()

validationFail :: [TomlItem] -> ValidationError -> SpecWith (Arg Expectation)
validationFail tomlItems validationError = it ("fail on " ++ show validationError) $
    validateItems tomlItems `shouldBe` Left validationError
