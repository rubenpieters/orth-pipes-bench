module Benchmarks.Spivey where

import Benchmarks.Common

import qualified Representations.Pipes as PipesBench
import qualified Representations.Conduit as ConduitBench
import qualified Representations.OrthPipes as OrthPipes
import qualified Representations.Streamly as StreamlyBench
import qualified Representations.ContPipe as ContPipe

import Criterion.Main

sieveL :: [Int] -> [Int]
sieveL (p:xs) = p : sieveL (Prelude.filter (\x -> x `mod` p /= 0) xs)

primesL :: Int -> [Int]
primesL n = Prelude.take n (sieveL [2..])

test :: IO ()
test = do
  check "pipes" PipesBench.collectPrimes
  check "conduit" ConduitBench.collectPrimes
  check "proxyrep" OrthPipes.collectPrimes
  check "streamly" StreamlyBench.collectPrimes

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
  , createBenchIO "streamly" "primes" l1 StreamlyBench.runPrimes
  , createBenchIO "pipes" "deep-pipe" (Prelude.take 5 l2) PipesBench.runDeepPipe
  , createBenchIO "conduit" "deep-pipe" (Prelude.take 5 l2) ConduitBench.runDeepPipe
  , createBenchIO "proxyrep" "deep-pipe" l2 OrthPipes.runDeepPipe
  , createBenchIO "contpipe" "deep-pipe" l2 (ContPipe.run "par2")
  , createBenchIO "pipes" "deep-seq" (Prelude.take 5 l2) PipesBench.runDeepSeq
  , createBenchIO "conduit" "deep-seq" l2 ConduitBench.runDeepSeq
  , createBenchIO "proxyrep" "deep-seq" l2 OrthPipes.runDeepSeq
  , createBenchIO "contpipe" "deep-seq" l2 (ContPipe.run "seq2")
  ]
  where
    l1 = [1000, 2500, 5000, 7500, 10000]
    l2 = [1000, 2500, 5000, 7500, 10000, 25000, 50000, 75000, 100000, 250000, 500000]
