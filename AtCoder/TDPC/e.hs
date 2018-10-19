import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

main :: IO ()
main = do
  d <- readLn
  n <- readLn
  print $ solve d n


-- Definitive solution
-- This is not efficient and too slow
solve :: Integer -> Integer -> Int
solve d n = length $ filter (\i -> (sum . digits $ i) `mod` d == 0) [2..n]

digits :: Integer -> [Integer]
digits = reverse . go
  where
    go 0 = []
    go n = (n `mod` 10):go (n `div` 10)

divConst = 10^9 + 7 :: Integer
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
moddiv x y = modmul x (power y (divConst - 2))
power x y
  | y == 0 = 1
  | y == 1 = x `mod` divConst
  | y `mod` 2 == 0 = (power x (y `div` 2))^2 `mod` divConst
  | otherwise = (power x (y `div` 2))^2 * x `mod` divConst
