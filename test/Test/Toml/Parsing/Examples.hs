module Test.Toml.Parsing.Examples
    ( parsingExamplesSpec
    ) where

import Data.Either (isRight)
import System.Directory (listDirectory)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import Toml.Parser (parse)

import qualified Data.Text.IO as TIO


parsingExamplesSpec :: Spec
parsingExamplesSpec = describe "Can parse official TOML examples" $ do
    files <- runIO $ listDirectory exampleDir
    mapM_ example files

example :: FilePath -> Spec
example file = it ("can parse file " ++ file) $ do
    toml <- TIO.readFile (exampleDir ++ file)
    isRight (parse toml) `shouldBe` True

exampleDir :: FilePath
exampleDir = "test/examples/"
