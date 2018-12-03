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
  n <- readLn
  print $ solve n

-- solve :: Int -> Int
solve n = traceShow mp $ (combination n4 2)*(n2-2)+n14*(n4-1)+n24*(n2-1)+n74
  where
    -- code below produce a wrong count map
    -- mp = toCountMap . primeFactors $ factorial n
    mp = facts n
    n2 = Map.size . Map.filter (>=2) $ mp
    n4 = Map.size . Map.filter (>=4) $ mp
    n14 = Map.size . Map.filter (>=14) $ mp
    n24 = Map.size . Map.filter (>=24) $ mp
    n74 = Map.size . Map.filter (>=74) $ mp

facts n = toCountMap . join . map primeFactors $ [1..n]

primeFactors n | n < 2 = []
primeFactors n = go n [2,3,5,7,11,13,17,19]
   where
     go !n pps@(p:ps)
       | n < p*p   = [n]
       | r > 0     = go n ps
       | otherwise = p:go q pps
      where
        (q,r) = quotRem n p
     go n [] = [n]

combination m i = (factorial m) `div` (factorial i) `div` (factorial (m - i))

toCountMap :: (Ord k, Integral a) => [k] -> Map.Map k a
toCountMap xs = Map.fromListWith (+) $ zip xs (repeat 1)

factorial n = foldr (*) 1 [1..n]



{-
solve n = traceShow candidates $ length . filter (<factorial n) . join $ candidates
-- solve n = 0
-}
candidates = do
  i <- [2..9]
  j <- [2..9]
  k <- [2..9]
  guard (i /= j && j /= k && k /= i)
  return [i^4*j^4*k^2, i^4*j^2*k^4, i^2*j^4*k^4]



{-
solve n = length . filter validate . nub $ candidates
  where
    candidates = map (foldr (*) 1) . subsequences . primeFactors . toInteger $ n
    -- validate :: [Int] -> Bool
    validate xs = [2,4,4] == sort . Map.elems . toCountMap $ xs
-}

-- primeFactors :: Integer -> [Integer]
