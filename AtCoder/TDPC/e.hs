import Data.Maybe
import Debug.Trace
import Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
  d <- readLn :: IO Int
  n <- readLn :: IO Integer
  print $ solveDp d n


-- solveDp :: Int -> Integer -> Integer
solveDp d n = dp!(orgk, True, 0) - 1
  where
    orgk = fromInteger . floor $ logBase 10 (fromIntegral n) :: Int
    ds = listArray (0, orgk) . reverse $ digits n :: Array Int Int
    dp :: Array (Int, Bool, Int) Integer
    dp = listArray ((0, False, 0), (orgk, True, d - 1)) $ map f candidates
    candidates = do
      digit <- [0..orgk]
      isCapped <- [False, True]
      m <- [0..(d - 1)]
      return (digit, isCapped, m)

    f :: (Int, Bool, Int) -> Integer
    f (0, False, m) = toInteger . length . filter (\i -> i `mod` d == m) $ [0..9]
    f (0, True, m) = toInteger . length . filter (\i -> i `mod` d == m) $ [0..(ds!0)]
    f (k, False, m) = foldr
      (\i acc -> modadd acc $ dp!(k-1, False, (m-i) `mod` d)) 0 [0..9]
    f (k, True, m) = dp!(k-1, True, (m-ak) `mod` d)
      `modadd` (foldr foldFunc 0 [0..(ak - 1)])
      where
        foldFunc i acc = modadd acc $ dp!(k-1, False, (m-i) `mod` d)
        ak = ds!k


-- Definitive solution
-- This is not efficient and too slow
-- solve :: Int -> Int -> Int
solve d n = length $ filter (\i -> (sum . digits $ i) `mod` d == 0) [2..n]

str2digits :: String -> [Int]
str2digits [] = []
str2digits (s:ss) = (read [s]):str2digits ss

digits :: Integer -> [Int]
digits = reverse . go
  where
    go 0 = []
    go n = (fromInteger $ n `mod` 10):go (n `div` 10)

divConst = 10^9 + 7 :: Integer
modadd x y = (x + y) `mod` divConst
