import Control.Monad

main :: IO ()
main = do
  (r:c:_) <- fmap (map read . words) getLine :: IO [Integer]
  (x:y:_) <- fmap (map read . words) getLine :: IO [Integer]
  (d:l:_) <- fmap (map read . words) getLine :: IO [Integer]
  let movingPattern = (r - x + 1) `modmul` (c - y + 1)
  print $ movingPattern `modmul` (count x y d l)

divConst = 10^9 + 7

factorial n
  | n <= 1 = 1
  | otherwise = n `modmul` factorial (n - 1)
invFactorial n
  | n <= 1 = 1
  | otherwise = (invFactorial (n - 1)) `moddiv` n

count x y d l = ac `modmul` dc `modmul` lc `modmul` nc
  where
    ac = factorial (x `modmul` y)
    dc = invFactorial d
    lc = invFactorial l
    nc = if (x * y) - d - l == 0
         then 1
         else countUnSatisfied x y d l
    -- TODO
    -- Count the number of alignments whose edge is blank
    countUnSatisfied x y d l = 1

modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
moddiv x y = modmul x (power y (divConst - 2))
power x y
  | y == 0 = 1
  | y == 1 = x `mod` divConst
  | y `mod` 2 == 0 = (power x (y `div` 2))^2 `mod` divConst
  | otherwise = (power x (y `div` 2))^2 * x `mod` divConst
