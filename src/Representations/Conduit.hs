module Representations.Conduit where

import Prelude hiding (filter, map, drop, take)

import Control.Monad

import Conduit
import Data.Conduit.List as C hiding (sinkNull, map, take, filter, mapM, concat)
import Data.Conduit.Combinators as C hiding (print)

{-# INLINE upfrom #-}
upfrom :: (Monad m) => Int -> ConduitT x Int m b
upfrom n = do
  yield n
  upfrom (n+1)

{-# INLINE sieve #-}
sieve :: (Monad m) => ConduitT Int Int m ()
sieve = do
  mP <- await
  case mP of
    Just p -> do
      yield p
      filter (\x -> x `mod` p /= 0) .| sieve
    Nothing -> return ()

{-# INLINE primes #-}
primes :: (Monad m) => Int -> ConduitT () Int m ()
primes n = upfrom 2 .| sieve .| take n

{-# INLINE iter #-}
iter :: Int -> (a -> a) -> a -> a
iter n f x = loop n x where
  loop k y = if k == 0 then y else loop (k-1) (f y)

{-# INLINE deepPipe #-}
deepPipe :: (Monad m) => Int -> ConduitT () Int m x
deepPipe n = iter n (forever (return ()) .|) (forever (yield 0))

{-# INLINE deepSeq #-}
deepSeq :: (Monad m) => Int -> ConduitT () Int m x
deepSeq n = iter n (>> forever (return ())) (forever (yield 0))

conduitRead :: (Read o) => ConduitT i o IO ()
conduitRead = forever $ do
  x <- lift readLn
  yield x

conduitShow :: (Show i) => ConduitT i o IO ()
conduitShow = do
  mX <- await
  case mX of
    Just x -> do
      lift (print x)
      conduitShow
    Nothing -> return ()

{-# INLINE runConduitIO #-}
runConduitIO :: (Read i, Show o) => ConduitT i o IO () -> IO ()
runConduitIO conduit = runConduit (conduitRead .| conduit .| conduitShow)

runConduitCollect :: ConduitT () o Identity () -> [o]
runConduitCollect p = runConduitPure $ p .| sinkList

{-# INLINE runPrimes #-}
runPrimes :: Int -> IO ()
runPrimes n = runConduitIO (primes n)

{-# INLINE runDeepPipe #-}
runDeepPipe :: Int -> IO ()
runDeepPipe n = runConduitIO (deepPipe n .| take n)

{-# INLINE runDeepSeq #-}
runDeepSeq :: Int -> IO ()
runDeepSeq n = runConduitIO (deepSeq n .| take n)

deepPipePure :: Int -> [Int]
deepPipePure n = runConduitPure (deepSeq n .| take n .| sinkList)

collectPrimes :: Int -> [Int]
collectPrimes n = runConduitCollect (primes n)

{-# INLINE source #-}
source :: Monad m => Int -> Int -> ConduitT () Int m ()
source from to = C.unfoldM step from
    where
    step cnt =
        if cnt > to
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE mapBench #-}
mapBench :: Monad m => Int -> m [Int]
mapBench n = runConduit (source 0 n .| C.map (+1) .| C.sinkList)

{-# INLINE mapMBench #-}
mapMBench :: Monad m => Int -> m [Int]
mapMBench n = runConduit (source 0 n .| C.mapM return .| C.sinkList)

{-# INLINE filterBench #-}
filterBench :: Monad m => Int -> m [Int]
filterBench n = runConduit (source 0 n .| C.filter even .| C.sinkList)

{-# INLINE concatBench #-}
concatBench :: Monad m => Int -> m [Int]
concatBench n = runConduit (source 0 n .| C.map (Prelude.replicate 3) .| C.concat .| C.sinkList)

{-# INLINE foldBench #-}
foldBench :: Monad m => Int -> m Int
foldBench n = runConduit (source 0 n .| C.foldl (+) 0)
