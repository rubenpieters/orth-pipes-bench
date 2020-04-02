module Representations.Loop where

{-# INLINE mapBench #-}
mapBench :: Monad m => Int -> m [Int]
mapBench n = go [] n
  where
    go :: Monad m => [Int] -> Int -> m [Int]
    go acc x | x < 0 = return acc
    go acc x = go ((x + 1) : acc) (x - 1)

{-# INLINE mapMBench #-}
mapMBench :: Monad m => Int -> m [Int]
mapMBench n = go [] n
  where
    go :: Monad m => [Int] -> Int -> m [Int]
    go acc x | x < 0 = return acc
    go acc x = go (x : acc) (x - 1)

{-# INLINE filterBench #-}
filterBench :: Monad m => Int -> m [Int]
filterBench n = go [] n
  where
    go :: Monad m => [Int] -> Int -> m [Int]
    go acc x | x < 0 = return acc
    go acc x | even x = go (x : acc) (x - 1)
    go acc x = go acc (x - 1)

{-# INLINE concatBench #-}
concatBench :: Monad m => Int -> m [Int]
concatBench n = go [] n
  where
    go :: Monad m => [Int] -> Int -> m [Int]
    go acc x | x < 0 = return acc
    go acc x = go (x : x : x : acc) (x - 1)

{-# INLINE foldBench #-}
foldBench :: Monad m => Int -> m Int
foldBench n = go 0 0
  where
    go :: Monad m => Int -> Int -> m Int
    go acc x | x > n = return acc
    go acc x = go (x + acc) (x + 1)
