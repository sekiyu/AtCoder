{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Functor
import Data.Array
-- import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]
readIntegers = map (fst . fromJust . B.readInteger) . B.words <$> B.getLine :: IO [Integer]
readVInts = map (fst . fromJust . B.readInt) . B.lines <$> B.getContents

main :: IO ()
main = do
  n <- readLn :: IO Int
  -- as <- replicateM n $ readLn :: IO [Int]
  as <- readVInts
  print $ solve''' n as


solve''' n (a:as) = go (IntMap.singleton a 1) (IntSet.singleton a) as
  where
    go :: IntMap.IntMap Int -> IntSet.IntSet -> [Int] -> Int
    go mp _ [] = sum $ IntMap.elems mp
    go mp bs (a:as)
      | a <= mn = go (IntMap.insertWith (+) a 1 mp) (IntSet.insert a bs) as
      | otherwise = go (IntMap.insertWith (+) a 1 newmp) (IntSet.insert a newbs) as
      where
        -- IntSetには最小値、最大値を求める関数がない
        -- そのため、最小値を求めるには、与えられうる最小値より小さい数(-1)を引数にluukupGTする
        mn = fromJust $ IntSet.lookupGT (-1) bs
        m = fromJust $ IntSet.lookupLT a bs
        newmp = IntMap.adjust (subtract 1) m mp
        newbs = if newmp IntMap.! m > 0 
          then bs
          else IntSet.delete m bs



solve'' n (a:as) = length $ foldl' go [a] as
  where
    go bs a
      | a <= mn = insert a bs
      | otherwise = replaceUpperBound (<a) a bs
      -- | otherwise = insert a $ deleteUpperBound (< a) bs
      where
        mn = head bs   

replaceUpperBound :: (a -> Bool) -> a -> [a] -> [a]
replaceUpperBound pred new (a:b:as) 
  | pred a && not (pred b) = new:b:as
  | not (pred a) = a:b:as
  | otherwise = a:(replaceUpperBound pred new (b:as))
replaceUpperBound pred new (a:[]) = if pred a then [new] else [a]
replaceUpperBound _ _ [] = []
  
deleteUpperBound :: (Ord a) => (a -> Bool) -> [a] -> [a]
deleteUpperBound pred (a:b:as)
  | pred a && not (pred b) = b:as
  | not (pred a) = a:b:as
  | otherwise = a:(deleteUpperBound pred (b:as))
deleteUpperBound pred (a:[]) = if pred a then [] else [a]
deleteUpperBound pred [] = []
        
        
-- solve' n (a:as) = length $ foldl' go [a] as
--   where
--     -- go bs [] = length bs
--     go bs a
--       | a <= mn = (a:bs)
--       | otherwise = (a:(delete m bs))
--       where
--         mn = minimum bs
--         m = maximum . filter (< a) $ bs
        
-- solve n (a:as) = go [a] as
--   where
--     go bs [] = length bs
--     go bs (a:as)
--       | a <= mn = go (a:bs) as
--       | otherwise = go (a:(delete m bs)) as
--       where
--         mn = minimum bs
--         m = maximum . filter (< a) $ bs
