{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OrthPipes where

import Prelude hiding (take, filter)

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))

newtype PCRep i o a = PCRep { unPCRep :: o -> PCRep o i a -> a }

type ProxyRep a' a b' b m r =
  PCRep a a' (m r) ->  -- request
  PCRep b' b (m r) ->  -- respond
  m r ->               -- exit
  m r

newtype ProxyC a' a b' b m x = ProxyC { unProxyC :: forall r.
    (x -> ProxyRep a' a b' b m r) -> ProxyRep a' a b' b m r
  }

instance Functor (ProxyC a' a b' b m) where
  fmap = liftM

instance Applicative (ProxyC a' a b' b m) where
  pure = return
  (<*>) = ap

instance Monad (ProxyC a' a b' b m) where
  return x = ProxyC (\k -> k x)
  p >>= f = ProxyC (\k -> unProxyC p (\x -> unProxyC (f x) k))

instance MonadTrans (ProxyC a' a b' b) where
    lift ma = ProxyC (\k req res e ->
      do a <- ma
         k a req res e
      )

yield :: a -> ProxyC x' x a' a m a'
yield a = ProxyC (\k req res e ->
    unPCRep res a (PCRep (\x res' ->
      k x req res' e
  )))

request :: a' -> ProxyC a' a y' y m a
request a' = ProxyC (\k req res e ->
    unPCRep req a' (PCRep (\x req' ->
      k x req' res e
  )))

await :: ProxyC () a y' y m a
await = request ()

exit :: ProxyC a' a b' b m x
exit = ProxyC (\_ _ _ e -> e)

mergeProxyRep :: (b' -> ProxyRep a' a b' b m r) -> ProxyRep b' b c' c m r -> ProxyRep a' a c' c m r
mergeProxyRep fb' p req res e = (mergeRR res e p . PCRep) (mergeRL req e . fb')
  where
  mergeRL :: PCRep a a' (m r) -> m r -> ProxyRep a' a b' b m r -> (PCRep b' b (m r) -> m r)
  mergeRL req e proxy res = proxy req res e
  mergeRR :: PCRep c' c (m r) -> m r -> ProxyRep b' b c' c m r -> (PCRep b b' (m r) -> m r)
  mergeRR res e proxy req = proxy req res e

(+>>) ::
  (b' -> ProxyC a' a b' b m r) ->
  ProxyC b' b c' c m r ->
  ProxyC a' a c' c m r
(+>>) fp q =
  ProxyC (\_ -> mergeProxyRep
    ((\p -> unProxyC p (\_ _ _ e -> e)) . fp)
    (unProxyC q (\_ _ _ e -> e))
  )

(>->) ::
  ProxyC a' a () b m r ->
  ProxyC () b c' c m r ->
  ProxyC a' a c' c m r
(>->) p1 p2 = (\() -> p1) +>> p2

--

type Pipe a b = ProxyC () a () b

upfrom :: (Monad m) => Int -> Pipe x Int m b
upfrom n = do
  yield n
  upfrom (n+1)

filter :: (Monad m) => (a -> Bool) -> Pipe a a m b
filter test = forever $
  do
    x <- await
    if test x
      then yield x
      else return ()

take :: (Monad m) => Int -> Pipe a a m ()
take n = if n == 0
  then exit
  else do
    x <- await
    yield x
    take (n - 1)

sieve :: (Monad m) => Pipe Int Int m x
sieve = do
  p <- await
  yield p
  filter (\x -> x `mod` p /= 0) >-> sieve

primes :: (Monad m) => Int -> Pipe () Int m ()
primes n = upfrom 2 >-> sieve >-> take n

iter :: Int -> (a -> a) -> a -> a
iter n f x = loop n x where
  loop k y = if k == 0 then y else loop (k-1) (f y)

deepPipe :: (Monad m) => Int -> Pipe () Int m x
deepPipe n = iter n (forever (return ()) >->) (forever (yield 0))

deepSeq :: (Monad m) => Int -> Pipe () Int m x
deepSeq n = iter n (>> forever (return ())) (forever (yield 0))

foldInput :: ((i -> a) -> a) -> PCRep i () a
foldInput inp = PCRep (\_ r -> inp (\i -> unPCRep r i (foldInput inp)))

foldOutput :: (o -> a -> a) -> PCRep () o a
foldOutput out = PCRep (\o prod -> out o (unPCRep prod () (foldOutput out)))

runPipesIO :: (Read i, Show o) => Pipe i o IO () -> IO ()
runPipesIO proxyc = unProxyC proxyc (\_ _ _ _ -> return ())
  (foldInput (\h -> do x <- readLn; h x))
  (foldOutput (\o q -> do print o; q))
  (return ())

runPrimes :: Int -> IO ()
runPrimes n = runPipesIO (primes n)

runDeepPipe :: Int -> IO ()
runDeepPipe n = runPipesIO (deepPipe n >-> take n)

runDeepSeq :: Int -> IO ()
runDeepSeq n = runPipesIO (deepSeq n >-> take n)

runPipesCollect :: Pipe () o Identity a -> [o]
runPipesCollect proxyc = runIdentity $ unProxyC proxyc (\_ _ _ _ -> return [])
  (foldInput (\h -> h ()))
  (foldOutput (\o (Identity q) -> return (o : q)))
  (return [])

deepPipePure :: Int -> [Int]
deepPipePure n = runPipesCollect (deepPipe n >-> take n)

deepSeqPure :: Int -> [Int]
deepSeqPure n = runPipesCollect (deepSeq n >-> take n)

collectPrimes :: Int -> [Int]
collectPrimes n = runPipesCollect (primes n)
