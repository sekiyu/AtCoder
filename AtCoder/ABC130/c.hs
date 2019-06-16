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
  ss <- map read . words <$> getLine :: IO [Double]
  let (ans, i) = solve ss
  putStr $ show ans
  putStr " "
  putStrLn $ show i

solve :: [Double] -> (Double, Int)
solve (w:h:x:y:_) = (w * h / 2.0, i)
  where
    i = if w / 2.0 == x && h / 2.0 == y
        then 1
        else 0