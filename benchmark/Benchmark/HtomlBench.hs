module Benchmark.HtomlBench
       ( decodeTextHtoml
       , decodeTomlHtoml
       , test
       ) where

import Data.Aeson.Types (parseEither, parseJSON)
import Data.Text (Text)
import Text.Toml
import Text.Toml.Types (Table)
import Benchmark.Type (Test (..))

import qualified Data.Text.IO as TIO
import qualified Text.Toml.Types as TTT


test :: IO ()
test = do
    txt <- TIO.readFile "./benchmark/benchmark.toml"
    case parseTomlDoc "" txt of
        Left _ -> error "Parsing failed"
        Right toml -> do
            TIO.putStrLn "Parsed toml"
            print toml
            TIO.putStrLn "JSON htoml"
            print $ TTT.toJSON toml
            case decodeTextHtoml txt of
                Left _ -> error "Conversion failed"
                Right decoded -> do
                    putStrLn "Decoded toml to Haskell type"
                    print decoded

decodeTextHtoml :: Text -> Either String Test
decodeTextHtoml txt = case parseTomlDoc "" txt of
    Left _ -> error "Parsing failed"
    Right toml -> decodeTomlHtoml toml

decodeTomlHtoml :: Table -> Either String Test
decodeTomlHtoml = parseEither parseJSON . TTT.toJSON