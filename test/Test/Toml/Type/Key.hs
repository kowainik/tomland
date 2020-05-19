module Test.Toml.Type.Key
    ( keySpec
    ) where

import Hedgehog (forAll, tripping)
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.Gen (genKey)
import Toml.Type.Key (KeysDiff (..), keysDiff)

import qualified Toml.Parser as Parser
import qualified Toml.Type.Printer as Printer


keySpec :: Spec
keySpec = describe "TOML Key" $ do
    keyRoundtripSpec
    keysDiffSpec

keyRoundtripSpec :: SpecWith (Arg Expectation)
keyRoundtripSpec = it "Key printing: fromString . prettyKey ≡ id" $ hedgehog $ do
    key <- forAll genKey
    tripping key Printer.prettyKey Parser.parseKey

keysDiffSpec :: Spec
keysDiffSpec = describe "Key difference" $ do
    it "Equal: Simple" $
        keysDiff "key" "key" `shouldBe` Equal
    it "Equal: Complex" $
        keysDiff "foo.bar.baz" "foo.bar.baz" `shouldBe` Equal
    it "NoPrefix: Simple" $
        keysDiff "foo" "bar" `shouldBe` NoPrefix
    it "NoPrefix: Only common suffix" $
        keysDiff "foo.key" "bar.key" `shouldBe` NoPrefix
    it "FstIsPref" $
        keysDiff "key" "key.nest" `shouldBe` FstIsPref "nest"
    it "SndIsPref" $
        keysDiff "key.nest" "key" `shouldBe` SndIsPref "nest"
    it "Diff: Simple" $
        keysDiff "key.foo" "key.bar" `shouldBe` Diff "key" "foo" "bar"
    it "Diff: Two components" $
        keysDiff "key.nest.foo" "key.nest.bar" `shouldBe` Diff "key.nest" "foo" "bar"
    it "Diff: Two diff components" $
        keysDiff "key.foo.nest" "key.bar.nest" `shouldBe` Diff "key" "foo.nest" "bar.nest"
    it "Diff: Different diff length" $
        keysDiff "key.foo" "key.bar.nest" `shouldBe` Diff "key" "foo" "bar.nest"
