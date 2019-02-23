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

main :: IO ()
main = do
  landscape <- getLine :: IO String
  let ans = solve' landscape :: [Int]
      ansShow = (length ans):ans
  print . sum $ ans
  putStrLn . unwords . map show $ ansShow

solve' :: String -> [Int]
solve' ls = go [] [(0, 0)] $ zip [0..] ls
  where
    go :: [Int] -> [(Int, Int)] -> [(Int, Char)] -> [Int]
    go _ ponds [] = tail . reverse . map snd $ ponds
    go [] ponds ((i, l):ls)
      | l == '\\' = go [i] ponds ls
      | otherwise = go [] ponds ls
    go stk@(s:tailStk) ponds ((i, l):ls)
      | l == '\\' = go (i:stk) ponds ls
      | l == '_' = go stk ponds ls
      | l == '/' = go tailStk newPonds ls
      where
        areaIncrement = i - s
        newPonds = unitePonds (s, areaIncrement) ponds

unitePonds :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
unitePonds (x, ax) ponds@((y, ay):tailPonds) = 
  if x < y 
  then unitePonds (x, ax + ay) tailPonds
  else (x, ax):ponds




toCoordinate :: String -> [Int]
toCoordinate ls = map (+ negate m) raws
  where
    raws = toLandscape ls
    m = minimum raws

toLandscape :: String -> [Int]
toLandscape = scanl' f 0
  where
    f :: Int -> Char -> Int
    f h c
      | c == '\\' = h - 1
      | c == '_' = h
      | c == '/' = h + 1
      | otherwise = traceShow c $ h

solve :: [Int] -> [Int]
solve ls = ls

