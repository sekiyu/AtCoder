import Control.Monad
import qualified Data.Map.Strict as Map
--import Data.HashMap.Strict


main :: IO ()
main = do
  n <- readLn
  as <- getLine
  print $ solve n as

solve :: Int -> String -> Int
solve n as = foldr
  (\ptn acc -> acc + if elem ptn rptns then 1 else 0) 0 ptns
  where
    ptns = colorPatterns . take n $ as
    rptns = colorPatterns . reverse $ drop n as

colorPatterns :: [a] -> [([a], [a])]
colorPatterns xs = zip blues reds
  where
   n = length xs
   iss = filterM (\_ -> [True, False]) [0..(n - 1)]
   blues = map (\is -> [ xs !! i| i <- is]) iss
   reds = map (\is -> [ xs !! i| i <- (subt [0..(n - 1)] is) ]) iss

subt :: (Ord a) => [a] -> [a] -> [a]
subt as [] = as
subt [] bs = []
subt (a:as) (b:bs)
  | a == b = subt as bs
  | a < b  = a:(subt as (b:bs))
  | a > b  = subt (a:as) bs

include :: String -> String -> Bool
include _ [] = True
include [] _ = False
include (a:as) (b:bs)
  | a == b = include as bs
  | otherwise = include as (b:bs)
