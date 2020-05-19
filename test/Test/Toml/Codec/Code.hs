module Test.Toml.Codec.Code
    ( codeSpec
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Toml.Codec.BiMap (TomlBiMapError (..))
import Toml.Codec.Code (decode)
import Toml.Codec.Error (TomlDecodeError (..))
import Toml.Parser (TomlParseError (..))
import Toml.Type.AnyValue (AnyValue (..), MatchError (..))
import Toml.Type.Value (TValue (..), Value (..))

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Toml.Codec as Toml


codeSpec :: Spec
codeSpec = describe "Codec.Code decode tests on different TomlDecodeErrors" $ do
    it "fails parse as unclosed bracket" $
        decode (Toml.int "a") "a = 'foo" `shouldBe`
            Left [ ParseError $ TomlParseError $ Text.unlines
                     [ "1:9:"
                     , "  |"
                     , "1 | a = 'foo"
                     , "  |         ^"
                     , "unexpected end of input"
                     , "expecting '''"
                     ]
                 ]
    it "fails decode text as Toml.int" $
        decode (Toml.int "a") "a = 'foo'" `shouldBe`
            Left [ matchErr TInteger $ AnyValue $ Text "foo"]
    -- table
    it "fails to decode table on missing field" $
        decode (Toml.table (Toml.int "x") "foo") "" `shouldBe`
            Left [ TableNotFound "foo"]
    it "fails to decode table on missing field" $
        decode (Toml.table (Toml.int "x") "foo") "[foo]" `shouldBe`
            Left [ KeyNotFound "foo.x"]

    -- lists

    it "fails to decode arrayOf when field is missing" $
        decode (Toml.arrayOf Toml._Int "foo") "" `shouldBe`
            Left [KeyNotFound "foo"]
    it "fails to decode arrayNonEmptyOf when field is missing" $
        decode (Toml.arrayNonEmptyOf Toml._Int "foo") "" `shouldBe`
            Left [KeyNotFound "foo"]
    it "fails to decode arrayNonEmptyOf when list is empty" $
        decode (Toml.arrayNonEmptyOf Toml._Int "foo") "foo = []" `shouldBe`
            Left [BiMapError $ ArbitraryError "Empty array list, but expected NonEmpty"]

    it "decodes to an empty list when field is missing" $
        decode (Toml.list (Toml.int "i") "foo") "" `shouldBe`
            Right []
    it "decodes to an empty list when empty list" $
        decode (Toml.list (Toml.int "i") "foo") "foo = []" `shouldBe`
            Right []
    it "fails to decode list when internal field is missing" $
        decode (Toml.nonEmpty (Toml.int "i") "foo") "foo = [{a = 42}]" `shouldBe`
            Left [KeyNotFound "foo.i"]
    it "fails to decode nonEmpty when field is missing" $
        decode (Toml.nonEmpty (Toml.int "i") "foo") "" `shouldBe`
            Left [TableArrayNotFound "foo"]
    it "fails to decode nonEmpty  when empty list" $
        decode (Toml.nonEmpty (Toml.int "i") "foo") "foo = []" `shouldBe`
            Left [TableArrayNotFound "foo"]

    -- map
    it "map: decodes to an empty map when field is missing" $
        decode (Toml.map (Toml.int "key") (Toml.text "val") "foo") "" `shouldBe`
            Right Map.empty
    it "map: fails to decode when key and value mismatch type" $
        decode
            (Toml.map (Toml.int "key") (Toml.text "val") "foo")
            "foo = [{key = 'a', val = 42}]"
            `shouldBe` Left
            [ matchErr TInteger $ AnyValue $ Text "a"
            , matchErr TText $ AnyValue $ Integer 42
            ]
    it "map: fails to decode when value is missing" $
        decode
            (Toml.map (Toml.int "key") (Toml.text "val") "foo")
            "foo = [{key = 42}]"
            `shouldBe` Left [ KeyNotFound "val"]

    it "tableMap: decodes to an empty map when field is missing" $
        decode (Toml.tableMap Toml._KeyText Toml.int "foo") "" `shouldBe`
            Right Map.empty
    it "tableMap: throws error on invalid key type" $
        decode (Toml.tableMap Toml._KeyInt Toml.text "foo")
            "foo = {a = 'a'}"
            `shouldBe` Left [BiMapError $ ArbitraryError "Prelude.read: no parse"]

    -- custom
    it "fails to decode via read when non-existing value" $
        decode (Toml.read @Ordering "foo") "foo = 'EQUAL'" `shouldBe`
            Left [BiMapError $ ArbitraryError "Prelude.read: no parse"]
    it "fails to decode via enumBounded when non-existing value" $
        decode (Toml.enumBounded @Ordering "foo") "foo = 'EQUAL'" `shouldBe`
            Left [BiMapError $ ArbitraryError "Value is 'EQUAL' but expected one of: LT, EQ, GT"]
    it "fails to validateIf" $
        decode (Toml.validateIf even Toml._Int "foo") "foo = 5" `shouldBe`
            Left [BiMapError $ ArbitraryError "Value does not pass the validation for key: foo"]
  where
    matchErr :: TValue -> AnyValue -> TomlDecodeError
    matchErr valueExpected valueActual = BiMapError $ WrongValue $ MatchError {..}
