{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ContTPipeR where

import Prelude hiding (take, filter)

import Data.Void
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))

newtype ProdPar  x a = ProdPar  { unProdPar  :: ConsPar x a -> a }
newtype ConsPar  x a = ConsPar  { unConsPar  :: x -> ProdPar x a -> a }

type ResultR i o a = ConsPar o a -> ProdPar i a -> a

newtype ContPipeR r i o m a = ContP { runContP :: (a -> ResultR i o (m r)) -> ResultR i o (m r) }

exit :: (Monad m) => ContPipeR () i o m a
exit = ContP (\_ _ _ -> return ())

type Pipe = ContPipeR

foldInput :: ((i -> a) -> a) -> ProdPar i a
foldInput inp = ProdPar (\r -> inp (\i -> unConsPar r i (foldInput inp)))

foldOutput :: (o -> a -> a) -> ConsPar o a
foldOutput out = ConsPar (\o prod -> out o (unProdPar prod (foldOutput out)))

runPipesIO :: (Read i, Show o) => Pipe () i o IO () -> IO ()
runPipesIO proxyc = runContP proxyc (\_ _ _ -> return ())
  (foldOutput (\o q -> do print o; q))
  (foldInput (\h -> do x <- readLn; h x))

runPipesCollect :: Pipe [o] () o Identity a -> [o]
runPipesCollect proxyc = runIdentity $ runContP proxyc (\_ _ _ -> return [])
  (foldOutput (\o (Identity q) -> return (o : q)))
  (foldInput (\h -> h ()))



{-

mergeResultR :: ResultR i m r -> ResultR m o r -> ResultR i o r
mergeResultR p q = \cons_o prod_i ->
  let q' prod = q cons_o prod
      p' cons = p cons prod_i
  in q' (ProdPar p')

mergeCont :: forall r i m o a. ContPipeR r i m a -> ContPipeR r m o a -> ContPipeR r i o a
mergeCont p q =
  ContP (\k ->
    let  p' :: ResultR i m r
         p' = runContP p undefined
         q' :: ResultR m o r
         q' = runContP q undefined
    in mergeResultR p' q')

instance Functor (ContPipeR r i o) where
  fmap = liftM

instance Applicative (ContPipeR r i o) where
  pure = return
  (<*>) = ap

instance Monad (ContPipeR r i o) where
  return x = ContP (\k -> k x)
  p >>= f = ContP (\k -> runContP p (\x -> runContP (f x) k))

--yield :: o -> ContPipeR r i o ()
yield o = ContP (\k out inp e ->
    unConsPar out o (ProdPar (\out' ->
      k () out' inp e
  )))

await :: ContPipeR r i o i
await = ContP (\k out inp e ->
    unProdPar inp (ConsPar (\x inp' ->
      k x out inp' e
  )))

exit :: ContPipeR r i o a
exit = ContP (\k out inp -> _)

(>->) p1 p2 = mergeCont p1 p2

--

type Pipe r a b = ContPipeR r a b

upfrom :: Int -> Pipe r x Int b
upfrom n = do
  yield n
  upfrom (n+1)

filter :: (a -> Bool) -> Pipe r a a b
filter test = forever $
  do
    x <- await
    if test x
      then yield x
      else return ()

take :: Int -> Pipe r a a ()
take n = if n == 0
  then exit
  else do
    x <- await
    yield x
    take (n - 1)

sieve :: Pipe r Int Int x
sieve = do
  p <- await
  yield p
  filter (\x -> x `mod` p /= 0) >-> sieve

primes :: Int -> Pipe r () Int ()
primes n = upfrom 2 >-> sieve >-> take n

iter :: Int -> (a -> a) -> a -> a
iter n f x = loop n x where
  loop k y = if k == 0 then y else loop (k-1) (f y)

deepPipe :: Int -> Pipe r () Int x
deepPipe n = iter n (forever (return ()) >->) (forever (yield 0))

deepSeq :: Int -> Pipe r () Int x
deepSeq n = iter n (>> forever (return ())) (forever (yield 0))

foldInput :: ((i -> a) -> a) -> ProdPar i a
foldInput inp = ProdPar (\r -> inp (\i -> unConsPar r i (foldInput inp)))

foldOutput :: (o -> a -> a) -> ConsPar o a
foldOutput out = ConsPar (\o prod -> out o (unProdPar prod (foldOutput out)))

runPipesIO :: (Read i, Show o) => Pipe r i o () -> IO ()
runPipesIO proxyc = runContP proxyc (\_ _ _ e -> e)
  (foldOutput (\o q -> do print o; q))
  (foldInput (\h -> do x <- readLn; h x))
  (return ())

runPrimes :: Int -> IO ()
runPrimes n = runPipesIO (primes n)

runDeepPipe :: Int -> IO ()
runDeepPipe n = runPipesIO (deepPipe n >-> take n)

runDeepSeq :: Int -> IO ()
runDeepSeq n = runPipesIO (deepSeq n >-> take n)

runPipesCollect :: Pipe r () o a -> [o]
runPipesCollect proxyc = runIdentity $ runContP proxyc (\_ _ _ e -> e)
  (foldOutput (\o (Identity q) -> return (o : q)))
  (foldInput (\h -> h ()))
  (return [])

deepPipePure :: Int -> [Int]
deepPipePure n = runPipesCollect (deepPipe n >-> take n)

deepSeqPure :: Int -> [Int]
deepSeqPure n = runPipesCollect (deepSeq n >-> take n)

collectPrimes :: Int -> [Int]
collectPrimes n = runPipesCollect (primes n)
-}
