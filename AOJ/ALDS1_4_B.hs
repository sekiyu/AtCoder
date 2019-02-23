import Data.List
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)
import Control.Monad
import Data.Functor
import qualified Data.IntSet as IntSet

readInts :: IO [Int]
readInts = map (fst . fromJust . BC.readInt) . BC.words <$> BC.getLine

main :: IO()
main = do
  _ <- fmap read getLine :: IO Int
  ss <- readInts
  _ <- fmap read getLine :: IO Int
  ts <- readInts
  -- print . solve ss $ sort ts
  print $ solve' ss ts

solve' :: [Int] -> [Int] -> Int
solve' ss ts = foldl' (\acc t -> acc + if IntSet.member t sSet then 1 else 0) 0 $ ts 
  where
    sSet = IntSet.fromList ss

--solve :: [Int] -> [Int] -> Int
--solve ss = foldl' (\acc t -> acc + binarySearch t ss 0 (length ss - 1)) 0
--solve ss ts = length [1 | t <- ts, binarySearch t ss 0 (length ss - 1)]
--solve _ [] n = n
--solve ss (t:ts) n = solve ss ts $ n + if elem t ss then 1 else 0
solve ss ts = length . filter (\t -> binarySearch t ss 0 (length ss - 1)) $ ts

binarySearch :: Int -> [Int] -> Int -> Int -> Bool
binarySearch t ss b e
  | t == mid = True
  | b == e || b > e  = False
  | t < mid = binarySearch t ss b (i - 1)
  | t > mid = binarySearch t ss (i + 1) e
  where
    i = div (b + e) 2
    mid = ss !! i
