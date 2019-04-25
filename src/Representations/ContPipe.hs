-- Coroutine pipelines
-- Copyright (c) J. M. Spivey 2017

{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances, Rank2Types #-}
-- ExistentialQuantification for the type of Effect in DirectPipe
-- FlexibleInstances for the instance Monad_f m => Monad (m i o)
-- Rank2Types for the Codensity stuff

module Representations.ContPipe where

import Prelude hiding (map, filter, take)
import qualified Data.List as List
import System.Environment
import System.IO(stdout, hFlush)
import Control.Applicative(Applicative)
import Control.Monad(liftM, ap)
import Data.IORef
       

-- Pipe interface

infixl 1 //

data Empty

class Monad_f pipe => PipeKit pipe where
  input :: pipe i o i
  output :: o -> pipe i o ()
  (//) :: pipe i h Empty -> pipe h o Empty -> pipe i o a
    -- The type variable a in the line above allows
    -- DirectPipe to be used in the implementation of HalfPipe
  effect :: IO a -> pipe i o a
  exit :: pipe i o a

class Monad_f m where
  return_f :: a -> m i o a
  bind_f :: m i o a -> (a -> m i o b) -> m i o b

instance Monad_f m => Monad (m i o) where
  return = return_f
  (>>=) = bind_f

instance Monad_f m => Functor (m i o) where fmap = liftM
instance Monad_f m => Applicative (m i o) where pure = return; (<*>) = ap

------------------------------------------------------------------

-- Direct-style implementation

data DirectPipe i o a =
    Input (i -> DirectPipe i o a)
  | Output o (DirectPipe i o a)
  | Done a
  | forall b . Effect (IO b) (b -> DirectPipe i o a)
  | Exit   

instance PipeKit DirectPipe where
  input = Input (\ x -> return x)

  output y = Output y skip

  p // q =
    case q of
      Input h -> p //* h
      Output y q' -> Output y (p // q')
      Done x -> error "terminated"
      Effect e h -> Effect e (\ x -> p // (h x))
      Exit -> Exit
    where
      infixl 1 //*
      p //* h =
        case p of
          Input k -> Input (\ x -> k x //* h)
          Output x p' -> p' // (h x)
          Done x -> error "terminated"
          Effect e k -> Effect e (\ x -> k x //* h)
          Exit -> Exit

  effect e = Effect e (\ x -> return x)

  exit = Exit

instance Monad_f DirectPipe where
  return_f x = Done x

  bind_f p f =
    case p of
      Input h -> Input (\ x -> h x >>= f)
      Output y p' -> Output y (p' >>= f)
      Done x -> f x
      Effect e h -> Effect e (\ x -> h x >>= f)
      Exit -> Exit

runDirPipe :: (Read i, Show o) => DirectPipe i o a -> IO ()
runDirPipe p =
  case p of
    Input h -> (do x <- readLn; runDirPipe (h x))
    Output x p -> (do print x; runDirPipe p)
    Done _ -> skip
    Effect e h -> do x <- e; runDirPipe (h x)
    Exit -> skip

------------------------------------------------------------------

-- Continuation-based implementation

newtype ContPipe i o a = MkPipe { runPipe :: Cont i o a -> Result i o }

type Cont i o a = a -> Result i o

type Result i o = InCont i -> OutCont o -> Answer

newtype InCont i = MkInCont { resume_in :: OutCont i -> Answer }
newtype OutCont o = MkOutCont { resume_out :: o -> InCont o -> Answer }

suspend_in :: Cont i o () -> InCont i -> InCont o
suspend_in k ik = MkInCont (\ ok -> k () ik ok)

suspend_out :: Cont i o i -> OutCont o -> OutCont i
suspend_out k ok = MkOutCont (\ x ik -> k x ik ok)

instance PipeKit ContPipe where
  input =
    MkPipe (\ k ik ok -> resume_in ik (suspend_out k ok))

  output x =
    MkPipe (\ k ik ok -> resume_out ok x (suspend_in k ik))

  p // q =
    MkPipe (\ k ik ok ->
      runPipe q k' (suspend_in (\ () -> runPipe p k') ik) ok)
    where k' _ _ _ = error "terminated"

  effect e = MkPipe (\ k ik ok -> do x <- e; k x ik ok)

  exit = MkPipe (\ k ik ok -> skip)

instance Monad_f ContPipe where
  return_f x = MkPipe (\ k -> k x)

  bind_f p f =
    MkPipe (\ k ->
      runPipe p (\ x -> runPipe (f x) k))

type Answer = IO ()

runContPipe :: (Read i, Show o) => ContPipe i o a -> IO ()
runContPipe p =
  runPipe p (\ _ _ _ -> skip) ik ok
  where
    ik = MkInCont (\ ok -> do x <- readLn; resume_out ok x ik)
    ok = MkOutCont (\ v ik -> do print v; resume_in ik ok)

------------------------------------------------------------------

-- Fork and join

-- Join without input
join0 :: ContPipe () h Empty -> ContPipe () o Empty -> ContPipe () (h, o) Empty
join0 p q =
  MkPipe (\ k ik ok ->
    loop (MkInCont (runPipe p k9 ik9)) (MkInCont (runPipe q k9 ik9)) ok)
  where
    loop ik1 ik2 ok =
      resume_in ik1 (MkOutCont (\ x ik1' ->
        resume_in ik2 (MkOutCont (\ y ik2' ->
          resume_out ok (x, y) (MkInCont (\ ok' ->
            loop ik1' ik2' ok'))))))

    k9 _ _ _ = error "terminated"
    ik9 = MkInCont (\ _ -> error "input forbidden in join")

-- Join with interleaved inputs
join :: ContPipe i h a -> ContPipe i o a -> ContPipe i (h, o) a
join p q =
  MkPipe (\ k ik ok ->
    do ir <- newIORef ik; or <- newIORef ok; body k ir or)
  where
    body k ir or =
      loop (MkInCont (runPipe p k0 ik0)) (MkInCont (runPipe q k0 ik0))
      where
        loop ik1 ik2 =
          resume_in ik1 (MkOutCont (\ x ik1' ->
            resume_in ik2 (MkOutCont (\ y ik2' ->
              do ok <- readIORef or;
                  resume_out ok (x, y) (MkInCont (\ ok' ->
                    do writeIORef or ok'; loop ik1' ik2'))))))

        k0 x _ _ =
          do ik <- readIORef ir; ok <- readIORef or; k x ik ok

        ik0 =
          MkInCont (\ ok' ->
            do ik <- readIORef ir; resume_in ik (MkOutCont (\ x ik' ->
                do writeIORef ir ik'; resume_out ok' x ik0)))

fork :: ContPipe i o Empty -> ContPipe i o Empty -> ContPipe i o Empty
fork p q =
  MkPipe (\ k ik ok -> (do or <- newIORef ok; body ik or))
  where
    body ik or =
      runPipe p k9 (MkInCont (\ ok1 ->
        runPipe q k9 (MkInCont (\ ok2 ->
          loop ik ok1 ok2)) ok0)) ok0

      where
        loop ik ok1 ok2 =
          resume_in ik (MkOutCont (\ v ik' -> 
            resume_out ok1 v (MkInCont (\ ok1' ->
              resume_out ok2 v (MkInCont (\ ok2' -> loop ik' ok1' ok2'))))))

        ok0 =
          MkOutCont (\ v ik' ->
            do ok <- readIORef or; resume_out ok v (MkInCont (\ ok' ->
                do writeIORef or ok'; resume_in ik' ok0)))

    k9 _ _ _ = error "terminated"

------------------------------------------------------------------

-- 'Half-hearted' improvement based on Voigtlaender

newtype HalfPipe i o a =
  MkHalf { apHalf :: forall b . (a -> DirectPipe i o b) -> DirectPipe i o b }

reify :: HalfPipe i o a -> DirectPipe i o a
reify p = apHalf p Done

instance PipeKit HalfPipe where
  input = MkHalf (\ k -> Input k)
  output x = MkHalf (\ k -> Output x (k ()))
  p // q = MkHalf (\ k -> reify p // reify q)
  effect e = MkHalf (\ k -> Effect e (\ x -> k x))
  exit = MkHalf (\ k -> Exit)

instance Monad_f HalfPipe where
  return_f x = MkHalf (\ k -> k x)
  bind_f p f =
    MkHalf (\ k -> apHalf p (\ x -> apHalf (f x) k))
    
runHalfPipe :: (Read i, Show o) => HalfPipe i o a -> IO ()
runHalfPipe p = runDirPipe (reify p)

------------------------------------------------------------------

-- Representation function

rep :: DirectPipe i o a -> ContPipe i o a

rep (Input h) =
  MkPipe (\ k ik ok ->
    let k' x = runPipe (rep (h x)) k in
    resume_in ik (suspend_out k' ok))

rep (Output v p) =
  MkPipe (\ k ik ok ->
    let k' () = runPipe (rep p) k in
    resume_out ok v (suspend_in k' ik))

rep (Done x) = return x -- MkPipe (\ k -> k x)
    
rep (Effect e h) =
  MkPipe (\ k ik ok -> do x <- e; runPipe (rep (h x)) k ik ok)

rep Exit = exit

------------------------------------------------------------------

-- Implementation-independent toolkit

skip :: Monad m => m ()
skip = return ()

forever :: Monad m => m a -> m b
forever p = q where q = p >> q

upfrom :: PipeKit pipe => Int -> pipe () Int a
upfrom n = do output n; upfrom (n+1)

filter :: PipeKit pipe => (u -> Bool) -> pipe u u a
filter test =
  forever (do x <- input; if test x then output x else skip)

take :: PipeKit pipe => Int -> pipe u u Empty
take n =
  if n == 0 then exit else do x <- input; output x; take (n-1)

map :: PipeKit pipe => (a -> b) -> pipe a b Empty
map f = forever (do x <- input; output (f x))

------------------------------------------------------------------

-- Examples

sieve :: PipeKit pipe => pipe Int Int Empty
sieve =
  do p <- input; output p; (filter (\ x -> x `mod` p /= 0)) // sieve

primes :: PipeKit pipe => Int -> pipe () Int Empty
primes n = upfrom 2 // sieve // take n

readLine :: PipeKit pipe => pipe u String ()
readLine = do s <- effect (do putStr "> "; hFlush stdout; getLine); output s

putLine :: PipeKit pipe => pipe String v ()
putLine = do s <- input; effect (putStrLn s)

rev :: PipeKit pipe => pipe () () ()
rev =
  forever readLine // map reverse // forever putLine

deep_pipe :: PipeKit pipe => Int -> pipe () Int Empty
deep_pipe n = iter n (forever skip //) (forever (output 0))

deep_seq :: PipeKit pipe => Int -> pipe () Int Empty
deep_seq n = iter n (>> forever skip) (forever (output 0))

iter :: Int -> (a -> a) -> a -> a
iter n f x = loop n x where
  loop k y = if k == 0 then y else loop (k-1) (f y)

sieve0 :: [Int] -> [Int]
sieve0 (p:xs) = p : sieve0 (List.filter (\ x -> x `mod` p /= 0) xs)

------------------------------------------------------------------

-- Main program

run :: String -> Int -> IO ()
run "primes0" n = mapM_ print (List.take n (sieve0 [2..]))
run "primes1" n = runDirPipe (primes n)
run "primes2" n = runContPipe (primes n)
run "primes3" n = runHalfPipe (primes n)
run "primes4" n = runContPipe (rep (primes n))

run "seq1" n = runDirPipe (deep_seq n // take n)
run "seq2" n = runContPipe (deep_seq n // take n)
run "seq3" n = runHalfPipe (deep_seq n // take n)

run "par1" n = runDirPipe (deep_pipe n // take n)
run "par2" n = runContPipe (deep_pipe n // take n)
run "par3" n = runHalfPipe (deep_pipe n // take n)

run "rev1" n = runDirPipe rev
run "rev2" n = runContPipe rev
    
main =
  do args <- getArgs;
      let n = read (args !! 1) :: Int in run (args !! 0) n
