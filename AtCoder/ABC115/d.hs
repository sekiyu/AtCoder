-- ABC115 D
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
  (n:x:_) <- fmap (map read . words) getLine :: IO [Integer]
  print $ solve n x

solve _ 0 = 0
solve n x
  | x == all = p n
  | x == (all `div` 2) + 1 = (solve n (x-1)) + 1
  | x <= all `div` 2 = solve (n-1) (x-1)
  | otherwise = p n - (solve n (all - x))
    where
      all = c n

c i = cs!i
  where
    cs = listArray (0, 50) $ map f [0..50]
    f 0 = 1
    f i = 2 * cs!(i-1) + 3

p i = ps!i
  where
    ps = listArray (0, 50) $ map f [0..50]
    f 0 = 1
    f i = 2 * ps!(i-1) + 1


{- slow
solve n x = length . filter id . take x $ burger n

burger i = burgers!i
  where
    burgers = listArray (0, 50) $ map f [0..50]
    f 0 = [True]
    f i = [False] ++ burgers!(i - 1) ++ [True] ++ burgers!(i - 1) ++ [False]
    
-}