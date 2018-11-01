module Benchmark.Main
       (main
       ) where

import Gauge.Main (bench, bgroup, defaultMain, nf)
import Text.Toml (parseTomlDoc)

import Benchmark.Tomland (codec)
import Toml.Parser (parse)

import qualified Data.Text.IO as TIO
import qualified Benchmark.Htoml as Htoml
import qualified Toml.Bi.Code as Tomland


-- | Benchmark.
main :: IO ()
main = do
    txt <- TIO.readFile "./benchmark/benchmark.toml"
    Right htomlVal <- pure $ parseTomlDoc "" txt
    Right tomlandVal <- pure $ parse txt
    defaultMain
        [ bgroup "Parse"
            [ bench "tomland" $ nf parse txt
            , bench "htoml"   $ nf (parseTomlDoc "") txt
            ]
        , bgroup "Convert"
            [ bench "tomland" $ nf (Tomland.runTomlCodec codec) tomlandVal
            , bench "htoml"   $ nf Htoml.convert htomlVal
            ]
        , bgroup "Decode"
            [ bench "tomland" $ nf (Tomland.decode codec) txt
            , bench "htoml"   $ nf Htoml.decode txt
            ]
        ]
