{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Representations.OrthPipes where

import Prelude hiding (filter, map, drop, take, mapM, concat)

import Control.Monad (forever, ap, liftM, when, (<$!>))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.Void

import Test.Inspection

import Data.IORef
import System.IO.Unsafe

newtype PCRep i o a = PCRep { unPCRep :: o -> PCRep o i a -> a }

type ProxyRep a' a b' b m r =
  PCRep a a' (m r) ->  -- request
  PCRep b' b (m r) ->  -- respond
  m r ->               -- exit
  m r

data ProxyC a' a b' b m x = ProxyC { unProxyC :: !(forall r.
    (x -> ProxyRep a' a b' b m r) -> ProxyRep a' a b' b m r
)}

instance Functor (ProxyC a' a b' b m) where
  fmap = liftM

instance Applicative (ProxyC a' a b' b m) where
  pure = return
  (<*>) = ap

instance Monad (ProxyC a' a b' b m) where
  return x = ProxyC (\k -> k x)
  p >>= f = ProxyC (\k -> unProxyC p (\x -> unProxyC (f x) k))

instance MonadTrans (ProxyC a' a b' b) where
  lift ma = ProxyC (\k req res e -> do
      a <- ma
      k a req res e
    )

instance (MonadIO m) => MonadIO (ProxyC a' a b' b m) where
  liftIO ma = ProxyC (\k req res e -> do
      a <- liftIO ma
      k a req res e
    )

{-# INLINE[1] yield #-}
yield :: a -> ProxyC x' x a' a m a'
yield a = ProxyC (\k req res e ->
    unPCRep res a (PCRep (\x res' ->
      k x req res' e
  )))

{-# INLINE[1] respond #-}
respond :: a -> ProxyC x' x a' a m a'
respond = yield

{-# INLINE request #-}
request :: a' -> ProxyC a' a y' y m a
request a' = ProxyC (\k req res e ->
    unPCRep req a' (PCRep (\x req' ->
      k x req' res e
  )))

{-# INLINE[1] await #-}
await :: ProxyC () a y' y m a
await = request ()

{-# INLINE exit #-}
exit :: ProxyC a' a b' b m x
exit = ProxyC (\_ _ _ e -> e)

pull :: a' -> ProxyC a' a a' a m r
pull = go
  where
    go a = do
      b <- request a
      c <- respond b
      go c

{-# INLINE mergeProxyRep #-}
mergeProxyRep ::
  (b' -> ProxyRep a' a b' b m r) ->
  ProxyRep b' b c' c m r ->
  ProxyRep a' a c' c m r
mergeProxyRep fp q = \req res e -> q (PCRep (\b' res' -> fp b' req res' e)) res e

{-# INLINE (+>>) #-}
(+>>) ::
  (b' -> ProxyC a' a b' b m r) ->
  ProxyC b' b c' c m r ->
  ProxyC a' a c' c m r
(+>>) fp q = ProxyC (\_ -> mergeProxyRep
    ((\p -> unProxyC p (\_ _ _ e -> e)) . fp)
    (unProxyC q (\_ _ _ e -> e))
  )

{-
ProxyC $ \k req res e -> unsafePerformIO $ do
  ref_req <- newIORef req
  ref_res <- newIORef res
  let p' p = unProxyC p (\a req' _ e' -> unsafePerformIO $ do res' <- readIORef ref_res ; return (k a req' res' e')) 
      q' = unProxyC q (\a _ res' e' -> unsafePerformIO $ do req' <- readIORef ref_req ; return (k a req' res' e'))
      modp p' req res e = p' (keepRef ref_req req) res e
      modq req res e = q' req (keepRef ref_res res) e
  return $ modq
    (PCRep (\b' res' -> modp (p' (fp b')) req res' e))
    res
    e
  where
    keepRef :: IORef (PCRep a' a r) -> PCRep a' a r -> PCRep a' a r
    keepRef ref (PCRep f) = PCRep $ \a (PCRep g) -> f a $ PCRep $ \a' k ->
      unsafePerformIO $ do
        modifyIORef' ref (\_ -> k)
        return (g a' (keepRef ref k))
-}

{-# INLINABLE (>+>) #-}
(fb' >+> fc') c' = fb' +>> fc' c'

{-# INLINE[1] (>->) #-}
(>->) ::
  ProxyC a' a () b m r ->
  ProxyC () b c' c m r ->
  ProxyC a' a c' c m r
(>->) p1 p2 = (\() -> p1) +>> p2

--

type Pipe a b = ProxyC () a () b
type Producer b = ProxyC Void () () b

{-# INLINE upfrom #-}
upfrom :: (Monad m) => Int -> Pipe x Int m b
upfrom n = do
  yield n
  upfrom (n+1)

{-# INLINE[1] filter #-}
filter :: (Monad m) => (a -> Bool) -> Pipe a a m b
filter test = forever $
  do
    x <- await
    if test x
      then yield x
      else return ()

{-# RULES
      "p >-> filter predicate" forall p predicate.
          p >-> filter predicate = for p (\a -> when (predicate a) (yield a))
    #-}
  

{-# INLINE take #-}
take :: (Monad m) => Int -> Pipe a a m ()
take n = if n <= 0
  then exit
  else do
    x <- await
    yield x
    take (n - 1)

{-# INLINE sieve #-}
sieve :: (Monad m) => Pipe Int Int m x
sieve = do
  p <- await
  yield p
  filter (\x -> x `mod` p /= 0) >-> sieve

{-# INLINE primes #-}
primes :: (Monad m) => Int -> Pipe () Int m ()
primes n = upfrom 2 >-> sieve >-> take n

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

foldInput :: ((i -> a) -> a) -> PCRep i () a
foldInput inp = PCRep (\_ r -> inp (\i -> unPCRep r i (foldInput inp)))

foldOutput :: (o -> a -> a) -> PCRep () o a
foldOutput out = PCRep (\o prod -> out o (unPCRep prod () (foldOutput out)))

{-# INLINE runPipesIO #-}
runPipesIO :: (Read i, Show o, Show a) => Pipe i o IO a -> IO ()
runPipesIO proxyc = unProxyC proxyc (\a _ _ _ -> putStrLn ("RETURN: " ++ show a))
  (foldInput (\h -> do x <- readLn; h x))
  (foldOutput (\o q -> do print o; q))
  (return ())

{-# INLINE runPrimes #-}
runPrimes :: Int -> IO ()
runPrimes n = runPipesIO (primes n)

{-# INLINE runDeepPipe #-}
runDeepPipe :: Int -> IO ()
runDeepPipe n = runPipesIO (deepPipe n >-> take n)

{-# INLINE runDeepSeq #-}
runDeepSeq :: Int -> IO ()
runDeepSeq n = runPipesIO (deepSeq n >-> take n)

{-# INLINE runPipesCollect #-}
runPipesCollect :: Pipe () o Identity a -> [o]
runPipesCollect proxyc = runIdentity $ unProxyC proxyc (\_ _ _ e -> e)
  (foldInput (\h -> h ()))
  (foldOutput (\o (Identity q) -> return (o : q)))
  (return [])

deepPipePure :: Int -> [Int]
deepPipePure n = runPipesCollect (deepPipe n >-> take n)

deepSeqPure :: Int -> [Int]
deepSeqPure n = runPipesCollect (deepSeq n >-> take n)

collectPrimes :: Int -> [Int]
collectPrimes n = runPipesCollect (primes n)

{-# INLINE[1] map #-}
map :: (a -> b) -> ProxyC () a () b m ()
map f = for cat (\x -> yield (f x))
  {-forever $ do
  x <- await
  yield (f x)-}

{-# RULES
  "p >-> map f" forall p f . p >-> map f = for p (\a -> yield (f a))

; "map f >-> p" forall p f . map f >-> p = (do
      a <- await
      return (f a) ) >~ p
#-}

{-# INLINE[1] cat #-}
cat :: ProxyC () a () a m r
cat = go () where
  go a = do
    b <- request a
    c <- yield b
    go c

{-# INLINE[1] mapM #-}
mapM :: Monad m => (a -> m b) -> ProxyC () a () b m r
mapM f = forever $ do
  x <- await
  b <- lift (f x)
  yield b

{-# RULES
  "p >-> mapM f" forall p f . p >-> mapM f = for p (\a -> do
      b <- lift (f a)
      yield b )

; "mapM f >-> p" forall p f . mapM f >-> p = (do
      a <- await
      b <- lift (f a)
      return b ) >~ p
#-}

{-# INLINE each #-}
each :: Foldable f => f a -> ProxyC x' x () a m ()
each = F.foldr (\a p -> yield a >> p) (return ())

{-# INLINE unfoldr #-}
unfoldr :: Monad m 
  => (s -> m (Either r (a, s))) -> s -> ProxyC x' x () a m r
unfoldr step = go where
  go s0 = do
    e <- lift (step s0)
    case e of
      Left r -> return r
      Right (a,s) -> do 
        _ <- yield a
        go s

{-# INLINE[1] concat #-}
concat :: Foldable f => ProxyC () (f a) () a m ()
concat = forever $ do
  fa <- await
  each fa

{-# RULES
  "p >-> concat" forall p . p >-> concat = for p each
#-}

{-# INLINABLE[0] for #-}
for ::
  ProxyC x' x b' b m a ->
  (b -> ProxyC x' x c' c m b') ->
  ProxyC x' x c' c m a
for p f = ProxyC $ \k req res e -> u $ do
  ref <- newIORef res
  return $ unProxyC p
    (\a req' _ e' -> u $ do res' <- readIORef ref ; return $ k a req' res' e')
    req
    (go ref req res e)
    e
    where
      go ref req res e = PCRep $ \m (PCRep g) ->
        unProxyC (f m) (\x req' res' e' -> u $ do modifyIORef' ref (\_ -> res') ; return $ g x (go ref req' res' e')) req res e
      u = unsafePerformIO

{-# RULES
    "for (for p f) g" forall p f g . for (for p f) g = for p (\a -> for (f a) g)

  ; "for p yield" forall p . for p yield = p

  ; "for (yield x) f" forall x f . for (yield x) f = f x

  ; "for cat f" forall f .
        for cat f =
            let go = do
                    x <- await
                    f x
                    go
            in  go

  ; "f >~ (g >~ p)" forall f g p . f >~ (g >~ p) = (f >~ g) >~ p

  ; "await >~ p" forall p . await >~ p = p

  ; "p >~ await" forall p . p >~ await = p

  ; "m >~ cat" forall m .
        m >~ cat =
            let go = do
                    x <- m
                    yield x
                    go
            in  go

  ; "p1 >-> (p2 >-> p3)" forall p1 p2 p3 .
        p1 >-> (p2 >-> p3) = (p1 >-> p2) >-> p3

  ; "p >-> cat" forall p . p >-> cat = p

  ; "cat >-> p" forall p . cat >-> p = p

#-}

{-# INLINE[1] (>~) #-}
(>~) ::
  ProxyC a' a () b m b ->
  ProxyC () b c' c m () ->
  ProxyC a' a c' c m ()
p >~ q = (\() -> go) +>> q
  where
    go = do a <- p; yield a; go

{-# INLINE foldProxyRep #-}
foldProxyRep ::
  (a' -> (a -> (m r)) -> (m r)) ->
  (b -> (b' -> (m r)) -> (m r)) ->
  m r ->
  ProxyRep a' a b' b m r -> m r
foldProxyRep req res e proxy = proxy (fromAlg req) (fromAlg res) e
  where
  fromAlg :: (o -> (i -> r) -> r) -> PCRep i o r
  fromAlg alg = PCRep (\o (PCRep r) -> alg o (\i -> r i (fromAlg alg)))

{-# INLINE runEffectPr #-}
runEffectPr :: (Monad m) => ProxyRep Void () () Void m () -> m ()
runEffectPr = foldResponsesPr (\_ _ -> ()) ()

{-# INLINE foldResponsesPr #-}
foldResponsesPr :: (Monad m) => (b -> o -> b) -> b -> ProxyRep x () () o m b -> m b
foldResponsesPr combine b proxy = foldProxyRep
  (\_ f -> f ())
  (\o f -> (`combine` o) <$!> (f ()))
  (return b)
  proxy

{-# INLINE construct #-}
construct :: ProxyC a' a b' b m x -> ProxyRep a' a b' b m r
construct (ProxyC plan) = plan (\_ _ _ e -> e)

{-# INLINE source #-}
source :: Monad m => Int -> Int -> ProxyC x' x () Int m ()
source from to = unfoldr step from
    where
    step cnt =
        if cnt > to
        then return (Left ())
        else return (Right (cnt, cnt + 1))

{-# INLINE mapBench #-}
mapBench :: Monad m => Int -> m [Int]
mapBench n = foldResponsesPr (\x y -> y : x) [] $ construct $ source 0 n >-> map (+1)

{-# INLINE mapMBench #-}
mapMBench :: Monad m => Int -> m [Int]
mapMBench n = foldResponsesPr (\x y -> y : x) [] $ construct $ source 0 n >-> mapM return

{-# INLINE filterBench #-}
filterBench :: Monad m => Int -> m [Int]
filterBench n = foldResponsesPr (\x y -> y : x) [] $ construct $ source 0 n >-> filter even

{-# INLINE concatBench #-}
concatBench :: Monad m => Int -> m [Int]
concatBench n = foldResponsesPr (\x y -> y : x) [] $ construct $ source 0 n >-> map (Prelude.replicate 3) >-> concat

{-# INLINE foldBench #-}
foldBench :: Monad m => Int -> m Int
foldBench n = foldResponsesPr (+) 0 (construct $ source 0 n)

-- fusion sanity check 

lhs :: Monad m => ProxyC x' x () Int m ()
lhs = source 0 1000 >-> map (+1) >-> map (+1)

rhs :: Monad m => ProxyC x' x () Int m ()
rhs = source 0 1000 >-> map (\x -> 2 + x)

inspect $ 'lhs === 'rhs

type Client a' a = ProxyC a' a () Void

type Server b' b = ProxyC Void () b' b
