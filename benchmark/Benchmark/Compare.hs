module Benchmark.Compare
       (main
       ) where

import Data.Text (Text)
import Gauge.Main (bench, bgroup, defaultMain, whnf)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types (Table)

import Benchmark.Htoml (convertHtoml, decodeHtoml)
import Benchmark.Tomland (codec)
import Toml.Bi.Code (decode, runTomlCodec)
import Toml.Parser (parse)
import Toml.Type (TOML)

import qualified Data.Text.IO as TIO


-- | Benchmark.
main :: IO ()
main = do
    (txt, htoml, tomland) <- preload
    defaultMain
        [ bgroup "tomland"
            [ bench "parse"   $ whnf parse txt
            , bench "convert" $ whnf (runTomlCodec codec) tomland
            , bench "decode"  $ whnf (decode codec) txt
            ]
        , bgroup "htoml"
            [ bench "parse"   $ whnf (parseTomlDoc "") txt
            , bench "convert" $ whnf convertHtoml htoml
            , bench "decode"  $ whnf decodeHtoml txt
            ]
        ]

-- | Preload toml file, and parsed version for libraries.
preload :: IO (Text, Table, TOML)
preload = do
    txt <- TIO.readFile "./benchmark/benchmark.toml"
    case parseTomlDoc "" txt of
        Left _      -> error "Htoml parsing failed"
        Right htoml ->
            case parse txt of
                Left _        -> error "Tomland parsing failed"
                Right tomland -> pure (txt, htoml, tomland)
