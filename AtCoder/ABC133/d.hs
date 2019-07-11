-- {-# LANGUAGE BangPatterns #-}
-- import Control.Monad
-- import Data.Maybe
-- import Debug.Trace
-- import Data.List
-- import qualified Data.Map.Strict as Map
-- import qualified Data.IntMap.Strict as IntMap
-- import qualified Data.Set as Set
-- import qualified Data.IntSet as IntSet
-- import Data.Functor
-- import Data.Array
-- -- import Data.Array.Unboxed
-- import Control.Monad.ST
-- import Data.Array.ST

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine :: IO [Integer]
  putStrLn . unwords . map show $ solve n as

solve n as = map (2*) $ x1 : (reverse . tail $ scanl' (\acc x -> x - acc) x1 . reverse $ tail as)
  where
    evens = map fst . filter (\(_, i) -> even i) $ zip as [1..]
    x1 = (sum as) `div` 2 - sum evens
