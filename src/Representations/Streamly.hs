{-# LANGUAGE FlexibleContexts #-}

module Representations.Streamly where

import Data.Functor.Identity
import Streamly
import qualified Streamly.Prelude as S
import Data.Function ((&))
import Control.Monad.Trans.Class (MonadTrans, lift)

{-# INLINE upfrom #-}
upfrom :: (Monad m, IsStream t) => Int -> t m Int
upfrom n = do
  n `S.cons` upfrom (n+1)

{-# INLINE sieve #-}
sieve :: (Monad m, IsStream t, MonadTrans t, Monad (t m)) => SerialT m Int -> t m Int
sieve s = do
  mResult <- lift $ S.uncons s
  case mResult of
    Nothing -> error "expected infinite stream"
    Just (p, s') -> p `S.cons` sieve (S.filter (\x -> x `mod` p /= 0) s')

{-# INLINE primes #-}
primes :: (Monad m, IsStream t, MonadTrans t, Monad (t m)) => Int -> t m Int
primes n =
    upfrom 2
  & sieve
  & S.take n

collectPrimes :: Int -> [Int]
collectPrimes n = runIdentity $ S.toList $ primes n

{-# INLINE runPrimes #-}
runPrimes :: Int -> IO ()
runPrimes n = S.mapM_ (\n -> print n) (primes n)

{-# INLINE source #-}
source :: MonadAsync m => Int -> Int -> SerialT m Int
source from to = S.unfoldrM (return . step) to
    where
    step cnt =
        if cnt < from
        then Nothing
        else Just (cnt, cnt - 1)

{-# INLINE mapBench #-}
mapBench :: MonadAsync m => Int -> m [Int]
mapBench n = S.foldl' (\x y -> y : x) [] (source 0 n & S.map (+1))

{-# INLINE mapMBench #-}
mapMBench :: MonadAsync m => Int -> m [Int]
mapMBench n = S.foldl' (\x y -> y : x) [] (source 0 n & S.mapM return)

{-# INLINE filterBench #-}
filterBench :: MonadAsync m => Int -> m [Int]
filterBench n = S.foldl' (\x y -> y : x) [] (source 0 n & S.filter even)

{-# INLINE concatBench #-}
concatBench :: MonadAsync m => Int -> m [Int]
concatBench n = S.foldl' (\x y -> y : x) [] (source 0 n & S.concatMap (S.replicate 3))

{-# INLINE foldBench #-}
foldBench :: MonadAsync m => Int -> m Int
foldBench n = S.foldl' (+) 0 (source 0 n)
