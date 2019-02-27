{-# LANGUAGE FlexibleContexts #-}

module Representations.Streamly where

import Data.Functor.Identity
import Streamly
import qualified Streamly.Prelude as S
import Data.Function ((&))
import Control.Monad.Trans.Class (MonadTrans, lift)

upfrom :: (Monad m, IsStream t) => Int -> t m Int
upfrom n = do
  n `S.cons` upfrom (n+1)

sieve :: (Monad m, IsStream t, MonadTrans t, Monad (t m)) => SerialT m Int -> t m Int
sieve s = do
  mResult <- lift $ S.uncons s
  case mResult of
    Nothing -> error "expected infinite stream"
    Just (p, s') -> p `S.cons` sieve (S.filter (\x -> x `mod` p /= 0) s')

primes :: (Monad m, IsStream t, MonadTrans t, Monad (t m)) => Int -> t m Int
primes n =
    upfrom 2
  & sieve
  & S.take n

collectPrimes :: Int -> [Int]
collectPrimes n = runIdentity $ S.toList $ primes n

runPrimes :: Int -> IO ()
runPrimes n = S.mapM_ (\n -> print n) (primes n)

{-# INLINE source #-}
source :: MonadAsync m => Int -> Int -> SerialT m Int
source from to = S.unfoldrM step from
    where
    step cnt =
        if cnt > to
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE mapBench #-}
mapBench :: MonadAsync m => Int -> m () 
mapBench n = runStream (source 0 n & S.map (+1))

{-# INLINE mapMBench #-}
mapMBench :: MonadAsync m => Int -> m () 
mapMBench n = runStream (source 0 n & S.mapM return)

{-# INLINE filterBench #-}
filterBench :: MonadAsync m => Int -> m () 
filterBench n = runStream (source 0 n & S.filter even)

{-# INLINE concatBench #-}
concatBench :: MonadAsync m => Int -> m () 
concatBench n = runStream (source 0 n & S.concatMap (S.replicate 3))
