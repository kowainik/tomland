module Test.Toml.Parser.Examples
    ( examplesSpec
    , parseUnparseSpec
    , parseUnparse
    ) where

import Data.Either (isRight)
import System.Directory (listDirectory)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import Toml.Parser (parse)
import Toml.Type.Printer (pretty)

import qualified Data.Text.IO as TIO

examplesSpec :: Spec
examplesSpec = describe "Can parse official TOML examples" $ do
    files <- runIO $ listDirectory exampleDir
    mapM_ example files

example :: FilePath -> Spec
example file = it ("can parse file " ++ file) $ do
    toml <- TIO.readFile (exampleDir ++ file)
    isRight (parse toml) `shouldBe` True

parseUnparseSpec :: Spec
parseUnparseSpec = describe "Can get same object after parse and unparse" $ do
    files <- runIO $ listDirectory exampleDir
    mapM_ parseUnparse files

parseUnparse :: FilePath -> Spec
parseUnparse file = it ("it will produce same text after parsing and unparsing " ++ file) $ do
    toml <- TIO.readFile (exampleDir ++ file)
    case parse toml of
       Left _ -> error "File can't be parsed"
       Right toml_obj -> (parse toml == ( parse $ pretty $ toml_obj)) `shouldBe` True

exampleDir :: FilePath
exampleDir = "test/examples/"

