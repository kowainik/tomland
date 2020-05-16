module Test.Toml.Type.Key
    ( keySpec
    ) where

import Data.String (IsString (..))
import Hedgehog (forAll, tripping)
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.Gen (genKey)
import Toml.Type.Key (KeysDiff (..), keysDiff)

import qualified Data.Text as Text
import qualified Toml.Type.Printer as Printer


keySpec :: Spec
keySpec = describe "TOML Key" $ do
    keyRoundtripSpec
    keysDiffSpec

keyRoundtripSpec :: SpecWith (Arg Expectation)
keyRoundtripSpec = it "Key printing: fromString . prettyKey ≡ id" $ hedgehog $ do
    key <- forAll genKey
    tripping key Printer.prettyKey (Just . fromString . Text.unpack)

keysDiffSpec :: Spec
keysDiffSpec = describe "Key difference" $ do
    it "Equal: Simple" $
        keysDiff "key" "key" `shouldBe` Equal
    it "Equal: Complex" $
        keysDiff "foo.bar.baz" "foo.bar.baz" `shouldBe` Equal
    it "NoPrefix" $
        keysDiff "foo" "bar" `shouldBe` NoPrefix
    it "FstIsPref" $
        keysDiff "key" "key.nest" `shouldBe` FstIsPref "nest"
    it "SndIsPref" $
        keysDiff "key.nest" "key" `shouldBe` SndIsPref "nest"
    it "Diff: Simple" $
        keysDiff "key.foo" "key.bar" `shouldBe` Diff "key" "foo" "bar"
    it "Diff: Two components" $
        keysDiff "key.nest.foo" "key.nest.bar" `shouldBe` Diff "key.nest" "foo" "bar"
