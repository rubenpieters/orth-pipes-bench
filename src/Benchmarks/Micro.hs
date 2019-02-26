module Benchmarks.Micro where

import Benchmarks.Common

import qualified Representations.Pipes as PipesBench
import qualified Representations.Conduit as ConduitBench
import qualified Representations.OrthPipes as OrthPipes
import qualified Representations.Streamly as StreamlyBench
import qualified Representations.ContPipe as ContPipe

import Criterion.Main

main :: IO ()
main = do
  criterion

criterion = defaultMain
  [ createBenchIO "pipes" "map" l1 PipesBench.mapBench
  , createBenchIO "proxyrep" "map" l1 OrthPipes.mapBench
  , createBenchIO "conduit" "map" l1 ConduitBench.mapBench
  , createBenchIO "streamly" "map" l1 StreamlyBench.mapBench
  ]
  where
    l1 = [1, 10000, 50000, 100000, 500000, 1000000]
