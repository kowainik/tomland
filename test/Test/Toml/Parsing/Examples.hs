module Test.Toml.Parsing.Examples where

import Data.Either (isRight)
import Test.Tasty.Hspec (Spec, describe, it, shouldReturn)
import Toml.Parser (parse)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec_Examples :: Spec
spec_Examples = describe "Can parse official TOML examples" $
    mapM_ example files

example :: FilePath -> Spec
example file = it ("can parse file " ++ file) $
      (isRight . parse <$> TIO.readFile ("test/examples/" ++ file))
      `shouldReturn` True

files :: [FilePath]
files =
    [ "fruit.toml"
    , "example-v0.3.0.toml"
    , "example-v0.4.0.toml"
    , "example.toml"
    , "hard_example.toml"
    , "hard_example_unicode.toml"
    ]
