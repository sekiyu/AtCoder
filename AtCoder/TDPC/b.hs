import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]

main :: IO ()
main = do
  (a:b:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- readInts
  bs <- readInts
  print $ solve a b as bs

solve :: Int -> Int -> [Int] -> [Int] -> Int
solve a b as bs = dp!(0, 0)
  where
    arraya = listArray (1, a) as :: UArray Int Int
    arrayb = listArray (1, b) bs :: UArray Int Int
    -- dp!(i,j)はa_i+1, b_j+1以降のカードだけが残っているときの取り得る最大得点
    -- おそらく配列の再利用でメモリの節約が可能？
    dp = listArray ((0, 0), (a, b)) $ map f [ (x,y) | x <- [0..a], y <- [0..b]] :: Array (Int, Int) Int
    f :: (Int, Int) -> Int
    f (i, j)
      | i == a && j == b = 0 -- nothing left
      | i == a-1 && j == b = arraya!a
      | i == a && j == b-1 = arrayb!b
      | i == a = arrayb!(j+1) + dp!(a, j+2) -- only Bs are left
      | j == b = arraya!(i+1) + dp!(i+2, b)-- only As are left
      | i == a-1 && j == b-1 = max (arraya!a) (arrayb!b)
      | i == a-1 = max
        (arraya!a + dp!(a, j+1))
        (arrayb!(j+1) + min (dp!(a, j+1)) (dp!(a - 1, j+2)))
      | j == b - 1 = max
        (arrayb!b + dp!(i+1, b))
        (arraya!(i+1) + min (dp!(i+2, b-1)) (dp!(i+1, b)))
      | otherwise = max
        (arraya!(i+1) + min (dp!(i+2, j)) (dp!(i+1, j+1)))
        (arrayb!(j+1) + min (dp!(i, j+2)) (dp!(i+1, j+1)))
