import Control.Monad
import Data.List

main :: IO ()
main = do
  (n:m:_) <- fmap (map read . words) getLine :: IO [Int]
  rs <- fmap (map read . words) getLine :: IO [Int]
  print . foldr train 0 . map fromIntegral . take m . reverse . sort $ rs

train v acc = (v + acc) / 2
