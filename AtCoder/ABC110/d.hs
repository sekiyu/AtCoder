-- ABC110 D Factorization
import qualified Data.IntSet as IntSet
import Data.List
import qualified Data.IntMap.Strict as IntMap

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  print $ solve (fromIntegral n) m

-- solve :: Int -> Int -> Int
solve n m = foldr (\x acc -> acc `modmul` (combination (fromIntegral x) n)) 1 counts
  where
    as = factorization m
    counts = IntSet.foldr (\x acc -> IntMap.insert x (length $ filter (==x) as) acc) IntMap.empty $ IntSet.fromList as


divConst = 10^9 + 7 :: Integer
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
moddiv x y = modmul x (power y (divConst - 2))
power x y
  | y == 0 = 1
  | y == 1 = x `mod` divConst
  | y `mod` 2 == 0 = (power x (y `div` 2))^2 `mod` divConst
  | otherwise = (power x (y `div` 2))^2 * x `mod` divConst
factorial n = foldr modmul 1 [1..n]
fact n m = foldr modmul 1 [(m+1)..n]

-- combination :: Int -> Int -> Int
combination a n = foldl' moddiv (fact (a + n - 1) (n - 1)) [1..a]

-- factors :: Int -> [Int]
factors n = [x | x <- [2..n], n `mod` x == 0]

-- factorization :: Int -> [Int]
factorization 1 = []
factorization x = let v = head $ factors x
                  in  v : factorization (x `div` v)
