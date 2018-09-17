module Main where

import PipesBench
import ConduitBench
import OrthPipes
import qualified ContPipe

import Criterion.Main

sieveL :: [Int] -> [Int]
sieveL (p:xs) = p : sieveL (Prelude.filter (\x -> x `mod` p /= 0) xs)

primesL :: Int -> [Int]
primesL n = Prelude.take n (sieveL [2..])

test :: IO ()
test = do
  check "pipes" PipesBench.collectPrimes
  check "conduit" ConduitBench.collectPrimes
  check "orth-pipes" OrthPipes.collectPrimes

check str f = if primesL testN == f testN
  then return ()
  else error ("test for " ++ str ++ " gave incorrect result")
  where
    testN = 500

main :: IO ()
main = do
  test
  criterion

criterion = defaultMain
  [ createBenchIO "pipes" "primes" l1 PipesBench.runPrimes
  , createBenchIO "conduit" "primes" l1 ConduitBench.runPrimes
  , createBenchIO "proxyrep" "primes" l1 OrthPipes.runPrimes
  , createBenchIO "contpipe" "primes" l1 (ContPipe.run "primes2")
  , createBenchIO "pipes" "deep-pipe" (Prelude.take 4 l2) PipesBench.runDeepPipe
  , createBenchIO "conduit" "deep-pipe" (Prelude.take 4 l2) ConduitBench.runDeepPipe
  , createBenchIO "proxyrep" "deep-pipe" l2 OrthPipes.runDeepPipe
  , createBenchIO "contpipe" "deep-pipe" l2 (ContPipe.run "par2")
  , createBenchIO "pipes" "deep-seq" (Prelude.take 4 l2) PipesBench.runDeepSeq
  , createBenchIO "conduit" "deep-seq" l2 ConduitBench.runDeepSeq
  , createBenchIO "proxyrep" "deep-seq" l2 OrthPipes.runDeepSeq
  , createBenchIO "contpipe" "deep-seq" l2 (ContPipe.run "seq2")
  ]
  where
    l1 = [1, 1000, 2500, 5000, 7500, 10000]
    l2 = [1, 1000, 2500, 5000, 7500, 10000, 25000, 50000, 75000, 100000, 250000, 500000]

createBenchPure groupName benchName ns benchmark =
  bgroup groupName (benchf <$> ns)
  where
    benchf n = bench (benchName ++ "/" ++ show n) (nf benchmark n)

createBenchIO groupName benchName ns benchmark =
  bgroup groupName (benchf <$> ns)
  where
    benchf n = bench (benchName ++ "/" ++ show n) (nfAppIO benchmark n)

