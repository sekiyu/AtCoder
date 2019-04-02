-- ABC122 D
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
  print $ solve n

-- solve :: Int -> Integer
solve n = if n < 4
          then naive n
          else foldl' modadd 0 . Map.elems $ foldl' proceed initial [5..n]
  where
    oks = [ w | w <- replicateM 4 acgt, not . or . map (flip isInfixOf w) $ ngwords ]
    initial = Map.fromList $ zip oks (repeat 1)
    -- proceed :: Map.Map String Int -> Int -> Map.Map String Int
    proceed m _ = Map.fromList [(ok, calc m ok) | ok <- oks ] 
    calc m key = foldl' modadd 0 $ map (m Map.!) c
      where
        c = [ x |  x <- map (\s -> tail key ++ [s]) acgt, notElem x ngwords, notElem (tail x) ngwords]

naive n = fromIntegral $ length $ [ w | w <- replicateM n acgt, not . or . map (flip isInfixOf w) $ ngwords ]

acgt = "ACGT"
ngwords = ["AGC", "GAC", "ACG", "ACGC", "AGGC", "ATGC", "AGAC", "AGGC", "AGTC"]

divConst = 10^9 + 7 :: Int
modadd x y = (x + y) `mod` divConst
modsub x y = (x - y) `mod` divConst
modmul x y = ((x `mod` divConst) * (y `mod` divConst)) `mod` divConst
