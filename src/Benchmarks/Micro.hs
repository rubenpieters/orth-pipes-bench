module Benchmarks.Micro where

import Benchmarks.Common

import qualified Representations.Pipes as PipesBench
import qualified Representations.Conduit as ConduitBench
import qualified Representations.OrthPipes as OrthPipes
import qualified Representations.Streamly as StreamlyBench
import qualified Representations.ContPipe as ContPipe

import Criterion.Main

test :: IO ()
test = do
  check "pipes map" PipesBench.mapBench (== mapResult)
  check "conduit map" ConduitBench.mapBench (== mapResult)
  check "proxyrep map" OrthPipes.mapBench (== mapResult)
  check "streamly map" StreamlyBench.mapBench (== mapResult)
  --
  check "pipes mapM" PipesBench.mapMBench (== mapMResult)
  check "conduit mapM" ConduitBench.mapMBench (== mapMResult)
  check "proxyrep mapM" OrthPipes.mapMBench (== mapMResult)
  check "streamly mapM" StreamlyBench.mapMBench (== mapMResult)
  --
  check "pipes filter" PipesBench.filterBench (== filterResult)
  check "conduit filter" ConduitBench.filterBench (== filterResult)
  check "proxyrep filter" OrthPipes.filterBench (== filterResult)
  check "streamly filter" StreamlyBench.filterBench (== filterResult)
  --
  check "pipes concat" PipesBench.concatBench (== concatResult)
  check "conduit concat" ConduitBench.concatBench (== concatResult)
  check "proxyrep concat" OrthPipes.concatBench (== concatResult)
  check "streamly concat" StreamlyBench.concatBench (== concatResult)
  --
  check "pipes fold" PipesBench.foldBench (== foldResult)
  check "conduit fold" ConduitBench.foldBench (== foldResult)
  check "proxyrep fold" OrthPipes.foldBench (== foldResult)
  check "streamly fold" StreamlyBench.foldBench (== foldResult)
  where
    mapResult = [1..1000001]
    mapMResult = [0..1000000]
    filterResult = filter even [0..1000000]
    concatResult = concatMap (replicate 3) [0..1000000]
    foldResult = sum [0..1000000]

check :: (Eq a) => String -> (Int -> IO a) -> (a -> Bool) -> IO ()
check str f checkF = do
  result <- f testN
  if checkF result
    then return ()
    else error ("test for " ++ str ++ " gave incorrect result")
  where
    testN = 1000000


main :: IO ()
main = do
  test
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
    l1 = [1000000]
