{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OrthPipesNoUnsafeCoerce where

import Prelude hiding (take, filter)

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))

import Unsafe.Coerce

newtype ReS i o a = ReS { unReS :: (o -> (i -> ReS i o a) -> a) -> a }
newtype MS m a = MS { unMS :: (m (MS m a) -> a) -> a}


newtype Proxy a' a b' b m r = Proxy { unProxy ::
    (a' -> (a -> ReS a a' (m r)) -> m r) -> -- request
    (b -> (b' -> ReS b' b (m r)) -> m r) -> -- respond
    m r ->                            -- exit
    m r
  }

newtype ProxyC a' a b' b m x = ProxyC { unProxyC :: forall r.
    (x -> Proxy a' a b' b m r) -> Proxy a' a b' b m r
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
    lift ma = ProxyC (\k -> Proxy (\req res e ->
      do a <- ma
         unProxy (k a) req res e
      ))

yield :: a -> ProxyC x' x a' a m a'
yield a = ProxyC (\k ->
  Proxy (\req res e -> res a (\x ->
    ReS (\res' -> unProxy (k x) req res' e))
  ))

request :: a' -> ProxyC a' a y' y m a
request a' = ProxyC (\k ->
  Proxy (\req res e -> req a' (\x ->
    ReS (\req' -> unProxy (k x) req' res e))
  ))

await :: ProxyC () a y' y m a
await = request ()

exit :: ProxyC a' a b' b m x
exit = ProxyC (\_ -> Proxy (\_ _ e -> e))

mergeLProxy ::
  (a' -> (a -> ReS a a' (m r)) -> m r) ->
  m r ->
  Proxy a' a b' b m r ->
  ReS b' b (m r)
mergeLProxy req e p = ReS (\res -> unProxy p req res e)

mergeRProxy ::
  (b -> (b' -> ReS b' b (m r)) -> m r) ->
  m r ->
  Proxy a' a b' b m r ->
  ReS a a' (m r)
mergeRProxy res e p = ReS (\req -> unProxy p req res e)

mergeProxy ::
  (b' -> Proxy a' a b' b m r) ->
  Proxy b' b c' c m r ->
  Proxy a' a c' c m r
mergeProxy fp q = Proxy (\req res e ->
    mergeReS (mergeRProxy res e q) (mergeLProxy req e . fp)
  )

mergeReS :: ReS i o a -> (o -> ReS o i a) -> a
mergeReS (ReS res) f = res (mergeReS . f)

(+>>) ::
  (b' -> ProxyC a' a b' b m r) ->
  ProxyC b' b c' c m r ->
  ProxyC a' a c' c m r
(+>>) fp q =
  ProxyC (\_ -> mergeProxy
    ((\p -> unProxyC p (\_ -> Proxy (\_ _ e -> e))) . fp)
    (unProxyC q (\_ -> Proxy (\_ _ e -> e)))
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

fixInpSP1 :: ((i -> a) -> a) -> (() -> (i -> ReS i () a) -> a)
fixInpSP1 inp _ h = inp (\i -> unReS (h i) (fixInpSP1 inp))

fixOutSP2 :: (o -> a -> a) -> (o -> (() -> ReS () o a) -> a)
fixOutSP2 out o h = out o (unReS (h ()) (fixOutSP2 out))

fixMMS :: (Monad m) => m (MS m (m a)) -> m a
fixMMS h = do
  h' <- h
  unMS h' fixMMS

runPipesIO :: (Read i, Show o) => Pipe i o IO () -> IO ()
runPipesIO proxyc = unProxy (unProxyC proxyc (\_ -> Proxy (\_ _ _ -> return ())))
  (fixInpSP1 (\h -> do x <- readLn; h x))
  (fixOutSP2 (\o q -> do print o; q))
  (return ())

runPrimes :: Int -> IO ()
runPrimes n = runPipesIO (primes n)

runDeepPipe :: Int -> IO ()
runDeepPipe n = runPipesIO (deepPipe n >-> take n)

runDeepSeq :: Int -> IO ()
runDeepSeq n = runPipesIO (deepSeq n >-> take n)

fixIMS :: Identity (MS Identity a) -> a
fixIMS x = unMS (runIdentity x) fixIMS

runPipesCollect :: Pipe () o Identity a -> [o]
runPipesCollect proxyc = runIdentity $ unProxy (unProxyC proxyc (\_ -> Proxy (\_ _ _ -> return [])))
  (fixInpSP1 (\h -> h ()))
  (fixOutSP2 (\o (Identity q) -> return (o : q)))
  (return [])

deepPipePure :: Int -> [Int]
deepPipePure n = runPipesCollect (deepPipe n >-> take n)

deepSeqPure :: Int -> [Int]
deepSeqPure n = runPipesCollect (deepSeq n >-> take n)

collectPrimes :: Int -> [Int]
collectPrimes n = runPipesCollect (primes n)
