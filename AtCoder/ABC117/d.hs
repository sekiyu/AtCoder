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
  (n:k:_) <- fmap (map read . words) getLine :: IO [Integer]
  as <- fmap (map read . words) getLine :: IO [Integer]
  print $ solve' k as

pad :: Int -> [Int] -> [Int]
pad n ls = ls ++ (take (n - m) (repeat 0))
  where m = length ls

solve' k as = traceShow maxn $ fromJust . maximum $ (map g [0..(maxn-1)]) ++ [zeros]
    where
      kbin = toRevBinary k
      m = length kbin  
      n = length as
      abinary = map toRevBinary as
      maxn = max (maximum $ map length abinary) m
      -- counts = map sum . transpose . map (pad m . toRevBinary) $ as :: [Int]
      counts = map sum . transpose . map (pad maxn) $ abinary :: [Int]
      maxs = [max c (n-c) | c <- counts]
      ks = map (\ki -> if ki == 0 then ki else n - ki) kbin
      zeros = Just . sum $ zipWith (*) counts bis

      bis = map (2^) [0..]
      g :: Int -> Maybe Int
      g i
        | (pad maxn kbin) !! i == 0 = Nothing
        | otherwise = return . sum 
          $ zipWith (*) ((take i ks) ++ [n - counts!!i] ++ (drop (i+1) maxs)) bis

toRevBinary = go 
  where
    go i
      | i < 2  = [fromIntegral (i `mod` 2)]
      | otherwise = (fromIntegral (i `mod` 2)):(go $ i `div` 2)
      
  {-
solve k as = (count (m-1) True kbin . transpose . map (slice m) $ abins) + count1s m abins 
  where
    n = length as
    abins = map toBinary as
    kbin = toBinary k
    m = length kbin
    count _ _ [] _ = 0
    count _ _ _ [] = 0
    count i True (k:kbin) (b:bins)
      | k == 0 = 2^i * (sum b) + count (i-1) True kbin bins
      | n <= 2 * (sum b) = 2^i * (sum b) + count (i-1) False kbin bins
      | otherwise = 2^i * (n - sum b) + count (i-1) True kbin bins
    count i False kbin (b:bins) = 2^i * (max (n - sum b) (sum b)) + count (i-1) False kbin bins

count1s m abins = sum . map f $ abins
    where 
      n = length abins
      f bs 
        | n <= m = 0
        | otherwise = sum . zipWith (\b i -> b * 2^i) (take (n - m) bs) $ [(n-1), (n-2)..]

-- toBinary :: Integer -> [Int]
toBinary = reverse . go 
  where
    go i
      | i < 2  = [fromIntegral (i `mod` 2)]
      | otherwise = (fromIntegral (i `mod` 2)):(go $ i `div` 2)

slice n ls 
  | m < n = (take (n - m) (repeat 0)) ++ ls
  | otherwise = drop (m - n) ls
  where m = length ls
-}

