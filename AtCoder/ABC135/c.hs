{-# LANGUAGE BangPatterns #-}
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
-- import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- readInts :: IO [Int]
  bs <- readInts :: IO [Int]
  print $ solve n as bs

solve :: Int -> [Int] -> [Int] -> Int
solve n as bs = x + min y (last as)
  where
    (x, y) = foldl' f (0, 0) $ zip as bs
    f :: (Int, Int) -> (Int, Int) -> (Int, Int)
    f !(!count, !left) !(!a, !b) =
      (
        count + min a (b + left),
        max 0 $ b - max 0 (a - left)
      )

