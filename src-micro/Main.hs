module Main where

import PipesBench
import ConduitBench
import OrthPipes
import qualified ContPipe
import qualified ContPipeR

import Criterion.Main

main :: IO ()
main = do
  criterion

criterion = defaultMain
  [ createBenchIO "pipes" "map" l1 PipesBench.mapBench
  , createBenchIO "proxyrep" "map" l1 OrthPipes.mapBench
  , createBenchIO "ConduitBench" "map" l1 ConduitBench.mapBench
  ]
  where
    l1 = [1, 10000, 50000, 100000, 500000, 1000000]

createBenchPure groupName benchName ns benchmark =
  bgroup groupName (benchf <$> ns)
  where
    benchf n = bench (benchName ++ "/" ++ show n) (nf benchmark n)

createBenchIO groupName benchName ns benchmark =
  bgroup groupName (benchf <$> ns)
  where
    benchf n = bench (benchName ++ "/" ++ show n) (nfAppIO benchmark n)