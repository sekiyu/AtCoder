import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

main :: IO ()
main = do
  (n:k:_) <- fmap (map read . words) getLine :: IO [Int]
  s <- getLine
  putStrLn $ solve k s

solve :: Int -> String -> String
solve k s = head . sort . filter validate $ permutations s
  where
    validate :: String -> Bool
    validate ss = (length . filter not $ zipWith (==) ss s) <= k
