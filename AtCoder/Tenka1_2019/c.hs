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
  n <- readLn :: IO Int
  s <- getLine :: IO String
  print $ solve s

white = '.'
black = '#'

solve :: String -> Int
solve s = minimum . map count $ scanl' (\(nwi, nbi) a -> if a == white then (nwi+1, nbi) else (nwi, nbi+1)) (0, 0) s
  where
    nw = length $ filter (==white) s
    -- nb = length $ filter (==black) s
    count (nwi, nbi) = nbi + nw - nwi
