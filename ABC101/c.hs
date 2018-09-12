import Data.List
import Control.Monad

main :: IO()
main = do
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (map read . words) getLine :: IO [Int]
  let
    Just ind = elemIndex 1 as
    count v k
      | v == k = 0
      | otherwise =  div v (k - 1) + if mod v (k - 1) == 0 then 0 else 1
    left = count ind k
    right = count (length as - ind - 1) k
  print (left + right)
