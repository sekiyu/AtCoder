import Data.List
import Control.Monad

main :: IO()
main = do
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (map read . words) getLine :: IO [Int]
  let
    Just ind = elemIndex 1 as
  print $ (div (n - k) (k - 1)) + 1 + if mod (n - k) (k - 1) == 0 then 0 else 1
