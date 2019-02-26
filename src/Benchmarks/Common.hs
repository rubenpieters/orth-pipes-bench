module Benchmarks.Common where

import Criterion.Main

import Control.DeepSeq (NFData)

createBenchPure :: (NFData a) => String -> String -> [Int] -> (Int -> a) -> Benchmark
createBenchPure groupName benchName ns benchmark =
  bgroup groupName (benchf <$> ns)
  where
    benchf n = bench (benchName ++ "/" ++ show n) (nf benchmark n)

createBenchIO :: (NFData a) => String -> String -> [Int] -> (Int -> IO a) -> Benchmark
createBenchIO groupName benchName ns benchmark =
  bgroup groupName (benchf <$> ns)
  where
    benchf n = bench (benchName ++ "/" ++ show n) (nfAppIO benchmark n)
