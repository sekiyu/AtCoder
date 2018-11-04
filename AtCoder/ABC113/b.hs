-- ABC113 B
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

main :: IO ()
main = do
  n <- readLn :: IO Int
  (t:a:_)<- fmap (map read . words) getLine :: IO [Int]
  hs <- fmap (map read . words) getLine :: IO [Int]
  print $ solve t a hs

solve t a hs = fst m
  where
    temps = zip [1..] $ map ((\x -> abs (x - (fromIntegral a))) . (temperature t)) hs
    m = minimumBy (\x y -> compare (snd x) (snd y)) temps


temperature :: Int -> Int -> Double
temperature t h = (fromIntegral t) - (fromIntegral h) * 0.006
