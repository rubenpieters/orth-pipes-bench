module Main where

import PipesBench
import OrthPipes
import OrthPipesNoUnsafeCoerce
import OrthPipesRecursiveMerge
import qualified ContPipe

import Criterion.Main

sieveL :: [Int] -> [Int]
sieveL (p:xs) = p : sieveL (Prelude.filter (\x -> x `mod` p /= 0) xs)

primesL :: Int -> [Int]
primesL n = Prelude.take n (sieveL [2..])

test :: IO ()
test = do
  check "pipes" PipesBench.collectPrimes
  check "orth-pipes" OrthPipes.collectPrimes
  check "safe-pipes" OrthPipesNoUnsafeCoerce.collectPrimes
  check "rec-pipes" OrthPipesRecursiveMerge.collectPrimes

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
  [ createBenchPure "pipes" "primes" l1 PipesBench.collectPrimes
  , createBenchPure "orth-pipes" "primes" l1 OrthPipes.collectPrimes
  , createBenchPure "orth-pipes-no-unsafe-coerce" "primes" l1 OrthPipesNoUnsafeCoerce.collectPrimes
  , createBenchPure "orth-pipes-recursive-merge" "primes" (Prelude.take 4 l1) OrthPipesRecursiveMerge.collectPrimes
  , createBenchIO "contpipe" "primes" l1 (ContPipe.run "primes2")
  , createBenchPure "pipes" "deep-pipe" (Prelude.take 5 l2) PipesBench.deepPipePure
  , createBenchPure "orth-pipes" "deep-pipe" l2 OrthPipes.deepPipePure
  , createBenchPure "orth-pipes-no-unsafe-coerce" "deep-pipe" l2 OrthPipesNoUnsafeCoerce.deepPipePure
  , createBenchPure "orth-pipes-recursive-merge" "deep-pipe" l2 OrthPipesRecursiveMerge.deepPipePure
  , createBenchIO "contpipe" "deep-pipe" l2 (ContPipe.run "par2")
  , createBenchPure "pipes" "deep-seq" (Prelude.take 5 l2) PipesBench.deepSeqPure
  , createBenchPure "orth-pipes-recursive-merge" "deep-seq" l2 OrthPipesRecursiveMerge.deepSeqPure
  , createBenchPure "orth-pipes-no-unsafe-coerce" "deep-seq" l2 OrthPipesNoUnsafeCoerce.deepSeqPure
  , createBenchPure "orth-pipes" "deep-seq" l2 OrthPipes.deepSeqPure
  , createBenchIO "contpipe" "deep-seq" l2 (ContPipe.run "seq2")
  ]
  where
    l1 = [1, 1000, 2500, 5000, 7500, 10000]
    l2 = [1, 1000, 2500, 5000, 7500, 10000, 25000, 50000, 75000, 100000, 250000, 500000, 750000, 1000000]

createBenchPure groupName benchName ns benchmark =
  bgroup groupName (benchf <$> ns)
  where
    benchf n = bench (benchName ++ "/" ++ show n) (nf benchmark n)

createBenchIO groupName benchName ns benchmark =
  bgroup groupName (benchf <$> ns)
  where
    benchf n = bench (benchName ++ "/" ++ show n) (nfIO (benchmark n))

