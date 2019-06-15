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

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine :: IO [Int]
  solve' n $ sort as

-- solve n as = do
--   print $ if minimum as > 0 
--     then sum as - 2 * head as
--     else if maximum as < 0
--       then (negate . sum $ as) + 2 * last as
--       else sum $ map abs as 
--   pre as
--   where
--     pre (a:b:as) = do
--       if b < 0 
--         then do
--           let l = last as
--           putStrLn . unwords . map show $ [l, a]
--           pre $ (b:(init as)) ++ [l - a]
--         else go (a:b:as)
    
--     go :: [Int] -> IO ()
--     go (a:as:[]) = do
--       putStrLn . unwords . map show $ [as, a]
--     go (a:as:ass) = do
--       putStrLn . unwords . map show $ [a, as]
--       go ((a-as):ass)
--     go _ = return ()
      
solve' n as = do
  print $ if minimum as > 0 
    then sum as - 2 * head as
    else if maximum as < 0
      then (negate . sum $ as) + 2 * last as
      else sum $ map abs as 
  putStr . unlines . map (unwords . map show) . unfoldr f $ (last as, as)
    where
      f :: (Int, [Int]) -> Maybe ([Int], (Int, [Int]))
      f (m, (a:b:[])) = Just ([m, a], (m - a, []))
      f (m, (a:b:as))
        | b < 0 = Just ([m, a], (m - a, b:as))
        | otherwise =  Just ([a, b], (m, (a-b):as))
      f _ = Nothing

