module StreamlyBench where

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