{-# LANGUAGE BangPatterns, FlexibleContexts #-}
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Functor
import qualified Data.Array as A
import qualified Data.Array.Unboxed as AU
import Control.Monad.ST
import Data.Array.ST
import Data.Char

import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map readInt . B.words <$> B.getLine :: IO [Int]
readIntegers = map (fst . fromJust . B.readInteger) . B.words <$> B.getLine :: IO [Integer]
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents
readVInts = map (fst . fromJust . B.readInt) . B.lines <$> B.getContents
readVTuple = map ((\(a:b:_) -> (readInt a, readInt b)) . B.words) . B.lines <$> B.getContents :: IO [(Int, Int)] 

main :: IO ()
main = do
  (n:q:_) <- map read . words <$> getLine :: IO [Int]
  -- abs <- replicateM (n - 1) $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
  -- pxs <- replicateM q $ (\(a:b:_) -> (a,b)) . map read . words <$> getLine :: IO [(Int, Int)]
  buff <- readVTuple :: IO [(Int, Int)]
  let (abs, pxs) = splitAt (n-1) buff
  putStrLn . unwords . map show $ solve' n q abs pxs

    
solve' n q abs pxs = AU.elems dfs
  where
    tree :: A.Array Int [Int]
    tree = runSTArray $ do
      t <- newArray (1, n) [] :: ST s (STArray s Int [Int])
      forM_ abs $ \(a, b) -> do
        aprev <- readArray t a
        writeArray t a $ b:aprev
        bprev <- readArray t b
        writeArray t b $ a:bprev
      return t

    countMap = runSTUArray $ do
      mp <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
      forM_ pxs $ \(p, x) -> do
        prev <- readArray mp p
        writeArray mp p $ prev + x
      return mp

    dfs :: AU.UArray Int Int 
    dfs = runSTUArray $ do
      c <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
      let 
        loop current parent point = do
          let p = point + countMap AU.! current
          writeArray c current p
          forM_ (tree A.! current) $ \next -> 
            if next == parent then return () else loop next current p

      loop 1 0 0
      return c

    -- tree = foldl' f (Map.singleton 1 []) abs :: Map.Map Int [Int]
    -- f t (a, b) = Map.insertWith (++) a [b] $ Map.insertWith (++) b [a] t
    -- countMap = foldl' g (IntMap.empty) pxs
    -- g counts (p, x) =  IntMap.insertWith (+) p x counts

      -- let 
      --   loop current parent point c = do
      --     let p = point + case (IntMap.lookup current countMap) of 
      --           Nothing -> 0 
      --           Just v -> v
      --     writeArray c current p
      --     forM_ (tree Map.! current) $ \next -> 
      --       if next == parent then return c else loop next current p c
      --     return c

      -- loop 1 0 0 c
        
-- solve n q abs pxs = elems arr
--   where
--     arr = listArray (1, n) $ map count [1..n]
--     count 1 = case IntMap.lookup 1 countMap of
--       Nothing -> 0
--       Just v -> v
--     count i = case IntMap.lookup i countMap of
--       Nothing -> arr!(tree IntMap.! i)
--       Just v -> v + arr!(tree IntMap.! i)
--     tree = foldl' f (IntMap.singleton 1 1) abs
--     f t (a, b) = IntMap.insert b a t
--     countMap = foldl' g (IntMap.empty) pxs
--     g counts (p, x) =  IntMap.insertWith (+) p x counts

