{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ContPipeR where

import Prelude hiding (take, filter)

import Data.Void
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))

newtype ProdPar  x a = ProdPar  { unProdPar  :: ConsPar x a -> a }
newtype ConsPar  x a = ConsPar  { unConsPar  :: x -> ProdPar x a -> a }

type ResultR i o = forall a. ConsPar o a -> ProdPar i a -> a -> a

newtype ContPipeR i o a  = ContP { runContP :: (a -> ResultR i o) -> ResultR i o }

mergeResultR :: ResultR i m -> ResultR m o -> ResultR i o
mergeResultR p q = \cons_o prod_i e ->
  let q' prod = q cons_o prod e
      p' cons = p cons prod_i e
  in q' (ProdPar p')

mergeCont :: forall i m o a. ContPipeR i m a -> ContPipeR m o a -> ContPipeR i o a
mergeCont p q =
  ContP (\k ->
    let  p' :: ResultR i m
         p' = runContP p undefined
         q' :: ResultR m o
         q' = runContP q undefined
    in mergeResultR p' q')

instance Functor (ContPipeR i o) where
  fmap = liftM

instance Applicative (ContPipeR i o) where
  pure = return
  (<*>) = ap

instance Monad (ContPipeR i o) where
  return x = ContP (\k -> k x)
  p >>= f = ContP (\k -> runContP p (\x -> runContP (f x) k))

yield :: o -> ContPipeR i o ()
yield o = ContP (\k out inp e ->
    unConsPar out o (ProdPar (\out' ->
      k () out' inp e
  )))

await :: ContPipeR i o i
await = ContP (\k out inp e ->
    unProdPar inp (ConsPar (\x inp' ->
      k x out inp' e
  )))

exit :: ContPipeR i o a
exit = ContP (\_ _ _ e -> e)

(>->) p1 p2 = mergeCont p1 p2

--

type Pipe a b = ContPipeR a b

upfrom :: Int -> Pipe x Int b
upfrom n = do
  yield n
  upfrom (n+1)

filter :: (a -> Bool) -> Pipe a a b
filter test = forever $
  do
    x <- await
    if test x
      then yield x
      else return ()

take :: Int -> Pipe a a ()
take n = if n == 0
  then exit
  else do
    x <- await
    yield x
    take (n - 1)

sieve :: Pipe Int Int x
sieve = do
  p <- await
  yield p
  filter (\x -> x `mod` p /= 0) >-> sieve

primes :: Int -> Pipe () Int ()
primes n = upfrom 2 >-> sieve >-> take n

iter :: Int -> (a -> a) -> a -> a
iter n f x = loop n x where
  loop k y = if k == 0 then y else loop (k-1) (f y)

deepPipe :: Int -> Pipe () Int x
deepPipe n = iter n (forever (return ()) >->) (forever (yield 0))

deepSeq :: Int -> Pipe () Int x
deepSeq n = iter n (>> forever (return ())) (forever (yield 0))

foldInput :: ((i -> a) -> a) -> ProdPar i a
foldInput inp = ProdPar (\r -> inp (\i -> unConsPar r i (foldInput inp)))

foldOutput :: (o -> a -> a) -> ConsPar o a
foldOutput out = ConsPar (\o prod -> out o (unProdPar prod (foldOutput out)))

runPipesIO :: (Read i, Show o) => Pipe i o () -> IO ()
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

runPipesCollect :: Pipe () o a -> [o]
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
