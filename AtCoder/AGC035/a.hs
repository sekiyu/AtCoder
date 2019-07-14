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
import Data.Bits

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine :: IO [Int]
  putStrLn $ if solve'' n as then "Yes" else "No"


solve'' n as = patternA || patternB || patternC
  where
    patternA = length keys == 3
      && keyCond
      && countCond
    patternB = all (0==) as
    patternC = (length keys) == 2
        && min fs sn == 0
        && (mp Map.! (min fs sn)) * 2 == mp Map.! (max fs sn)
    mp = toCountMap as
    keys = Map.keys mp
    (a:b:c:_) = keys
    (fs:sn:_) = keys
    keyCond = xor a b == c && xor b c == a && xor c a == b 
    countCond = mp Map.! a == mp Map.! b && mp Map.! a == mp Map.! c
  

toCountMap :: (Ord k) => [k] -> Map.Map k Int
toCountMap = Map.fromListWith (+) . flip zip (repeat 1)
    
-- solve' n as
--   | length keys /= 3 = False
--   | keyCond = countCond
--   | otherwise = False
--   where
--     mp = toCountMap as
--     keys = Map.keys mp
--     (a:b:c:_) = keys
--     keyCond = xor a b == c && xor b c == a && xor c a == b 
--     countCond = mp Map.! a == mp Map.! b && mp Map.! a == mp Map.! c

-- solve n as = f as mp
--   -- f (fs:sn:(delete fs $ delete sn as)) mp
--   -- any id [ True | a <- tail as, f ((head as):a:(delete a (tail as))) (Map.adjust (\y -> y-1) a mp) ]
--   -- f as mp
--   -- go as set
--   where
--     keys = Map.keys mp
--     (fs:sn:_) = keys
--     mp = toCountMap as
--     f :: [Int] -> Map.Map Int Int -> Bool
--     f (a:b:[]) _ = (a `xor` b) == fs && (last as) `xor` fs == sn
--     f (a:b:bs) m
--       | Map.member x m = traceShow m $ if m Map.! x > 0 
--           then f (b:bs) $ Map.adjust (\y -> y-1) x m
--           else False
--       | otherwise = False
--       where x = (a `xor` b)

--     prod = foldl1' xor as
--     n = length as
--     odds = all (\a -> prod `xor` a == a) as
--     evens = all (\a -> prod `xor` a == 0) as
--     first = if odd n
--       then odds
--       else evens
  
-- --     -- set = IntSet.delete (as!!1) . IntSet.delete (as!!0) $ IntSet.fromList as
-- --     -- go (a:b:[]) _ = (a `xor` b) == head as
-- --     -- go (a:b:as) s = if IntSet.member m s
-- --     --     then go (b:as) $ IntSet.delete m s
-- --     --     else False
-- --     --   where
-- --     --     m = a `xor` b

