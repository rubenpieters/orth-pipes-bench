{-# LANGUAGE TemplateHaskell #-}

module Representations.Pipes where

import Prelude hiding (filter, map, drop, take)

import Data.Functor.Identity
import Pipes
import qualified Pipes.Prelude as P
import Control.Monad (forever)
import Pipes.Internal

import Test.Inspection

{-# INLINE upfrom #-}
upfrom :: (Monad m) => Int -> Pipe x Int m b
upfrom n = do
  yield n
  upfrom (n+1)

{-# INLINE sieve #-}
sieve :: (Monad m) => Pipe Int Int m x
sieve = do
  p <- await
  yield p
  P.filter (\x -> x `mod` p /= 0) >-> sieve

{-# INLINE primes #-}
primes :: (Monad m) => Int -> Pipe () Int m ()
primes n = upfrom 2 >-> sieve >-> P.take n

{-# INLINE iter #-}
iter :: Int -> (a -> a) -> a -> a
iter n f x = loop n x where
  loop k y = if k == 0 then y else loop (k-1) (f y)

{-# INLINE deepPipe #-}
deepPipe :: (Monad m) => Int -> Pipe () Int m x
deepPipe n = iter n (forever (return ()) >->) (forever (yield 0))

{-# INLINE deepSeq #-}
deepSeq :: (Monad m) => Int -> Pipe () Int m x
deepSeq n = iter n (>> forever (return ())) (forever (yield 0))

{-# INLINE runPipesIO #-}
runPipesIO :: (Read i, Show o) => Pipe i o IO a -> IO a
runPipesIO (Request _ h) = do x <- readLn; runPipesIO (h x)
runPipesIO (Respond o q) = do print o; runPipesIO (q ())
runPipesIO (M e) = do p <- e; runPipesIO p
runPipesIO (Pure r) = return r

{-# INLINE runPrimes #-}
runPrimes :: Int -> IO ()
runPrimes n = runPipesIO (primes n)

{-# INLINE runDeepPipe #-}
runDeepPipe :: Int -> IO ()
runDeepPipe n = runPipesIO (deepPipe n >-> P.take n)

{-# INLINE runDeepSeq #-}
runDeepSeq :: Int -> IO ()
runDeepSeq n = runPipesIO (deepSeq n >-> P.take n)

runPipesCollect :: Pipe () o Identity a -> [o]
runPipesCollect (Request _ h) = runPipesCollect (h ())
runPipesCollect (Respond o q) = o : runPipesCollect (q ())
runPipesCollect (M e) = runPipesCollect (runIdentity e)
runPipesCollect (Pure _) = []

deepPipePure :: Int -> [Int]
deepPipePure n = runPipesCollect (deepPipe n >-> P.take n)

deepSeqPure :: Int -> [Int]
deepSeqPure n = runPipesCollect (deepSeq n >-> P.take n)

collectPrimes :: Int -> [Int]
collectPrimes n = runPipesCollect (primes n)

{-# INLINE source #-}
source :: Monad m => Int -> Int -> Producer Int m ()
source from to = P.unfoldr step from
    where
    step cnt =
        if cnt > to
        then return (Left ())
        else return (Right (cnt, cnt + 1))

{-# INLINE mapBench #-}
mapBench :: Monad m => Int -> m [Int]
mapBench n = P.fold (\x y -> y : x) [] id (source 0 n >-> P.map (+1) >-> forever await)

{-# INLINE mapMBench #-}
mapMBench :: Monad m => Int -> m [Int]
mapMBench n = P.fold (\x y -> y : x) [] id (source 0 n >-> P.mapM return >-> forever await)

{-# INLINE filterBench #-}
filterBench :: Monad m => Int -> m [Int]
filterBench n = P.fold (\x y -> y : x) [] id (source 0 n >-> P.filter even >-> forever await)

{-# INLINE concatBench #-}
concatBench :: Monad m => Int -> m [Int]
concatBench n = P.fold (\x y -> y : x) [] id (source 0 n >-> P.map (Prelude.replicate 3) >-> P.concat >-> forever await)

{-# INLINE foldBench #-}
foldBench :: Monad m => Int -> m Int
foldBench n = P.fold (+) 0 id (source 0 n)
