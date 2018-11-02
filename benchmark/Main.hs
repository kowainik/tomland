module Main
       (main
       ) where

import Gauge.Main (bench, bgroup, defaultMain, nf)

import qualified Benchmark.Htoml as Htoml
import qualified Benchmark.Tomland as Tomland
import qualified Data.Text.IO as TIO
import qualified Toml.Parser as Tomland


-- | Benchmark.
main :: IO ()
main = do
    txt <- TIO.readFile "./benchmark/benchmark.toml"
    Right htomlVal <- pure $ Htoml.parse txt
    Right tomlandVal <- pure $ Tomland.parse txt
    defaultMain
        [ bgroup "Parse"
            [ bench "tomland" $ nf Tomland.parse txt
            , bench "htoml"   $ nf Htoml.parse txt
            ]
        , bgroup "Convert"
            [ bench "tomland" $ nf Tomland.convert tomlandVal
            , bench "htoml"   $ nf Htoml.convert htomlVal
            ]
        , bgroup "Decode"
            [ bench "tomland" $ nf Tomland.decode txt
            , bench "htoml"   $ nf Htoml.decode txt
            ]
        ]
