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
  , createBenchIO "pipes" "mapM" l1 PipesBench.mapMBench
  , createBenchIO "proxyrep" "mapM" l1 OrthPipes.mapMBench
  , createBenchIO "conduit" "mapM" l1 ConduitBench.mapMBench
  , createBenchIO "streamly" "mapM" l1 StreamlyBench.mapMBench
  , createBenchIO "pipes" "filter" l1 PipesBench.filterBench
  , createBenchIO "proxyrep" "filter" l1 OrthPipes.filterBench
  , createBenchIO "conduit" "filter" l1 ConduitBench.filterBench
  , createBenchIO "streamly" "filter" l1 StreamlyBench.filterBench
  , createBenchIO "pipes" "concat" l1 PipesBench.concatBench
  , createBenchIO "proxyrep" "concat" l1 OrthPipes.concatBench
  , createBenchIO "conduit" "concat" l1 ConduitBench.concatBench
  , createBenchIO "streamly" "concat" l1 StreamlyBench.concatBench
  , createBenchIO "pipes" "fold" l1 PipesBench.foldBench
  , createBenchIO "proxyrep" "fold" l1 OrthPipes.foldBench
  , createBenchIO "conduit" "fold" l1 ConduitBench.foldBench
  , createBenchIO "streamly" "fold" l1 StreamlyBench.foldBench
  ]
  where
    l1 = [1, 10000, 50000, 100000, 500000, 1000000]
