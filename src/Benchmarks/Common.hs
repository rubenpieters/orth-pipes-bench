module Benchmarks.Common where

import Criterion.Main

createBenchPure groupName benchName ns benchmark =
  bgroup groupName (benchf <$> ns)
  where
    benchf n = bench (benchName ++ "/" ++ show n) (nf benchmark n)

createBenchIO groupName benchName ns benchmark =
  bgroup groupName (benchf <$> ns)
  where
    benchf n = bench (benchName ++ "/" ++ show n) (nfAppIO benchmark n)
