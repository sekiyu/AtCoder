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

main :: IO ()
main = do
  (n:k:_) <- map read . words <$> getLine :: IO [Int]
  s <- getLine :: IO String
  print $ solve n k s

solve n k s = go k counts
  where
    counts = unfoldr f s
    f :: String -> Maybe (Int, String)
    f [] = Nothing
    f (s:ss) = Just (1 + (length . takeWhile (==s) $ ss), dropWhile (==s) ss)

    go 0 cs = (sum cs) - (length cs)
    go i (c0:c1:c2:cs) = go (i-1) $ (c0+c1+c2):cs
    go _ cs = (sum cs) - 1
