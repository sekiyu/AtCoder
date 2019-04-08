{-# LANGUAGE BangPatterns #-}
import Data.List
import Control.Monad
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInteger = toInteger . fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine :: IO [Int]
read2dInts = map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents


main :: IO ()
main = do
  (a:b:q:_) <- fmap (map read . words) getLine :: IO [Int]
  ss <- replicateM a $ readInteger <$> B.getLine :: IO [Integer]
  ts <- replicateM b $ readInteger <$> B.getLine :: IO [Integer]
  qs <- replicateM q $ readInteger <$> B.getLine :: IO [Integer]
  mapM_ (print . solve a b ss ts) qs

maxB = 10^15
minB = negate maxB

solve a b ss ts = go
  where
    sarr = listArray (0, a+1) $ (minB:sort ss) ++ [maxB] :: Array Int Integer
    tarr = listArray (0, b+1) $ (minB:sort ts) ++ [maxB] :: Array Int Integer
    go q = minimum candidates
      where
        candidates = [
          (max sr tr) - q, 
          sr - tl + sr - q, 
          sr - tl + q - tl, 
          tr - sl + tr - q,
          tr - sl + q - sl, 
          q - (min sl tl)
          ]
        s = lowerBound' (\i -> sarr!i > q) (0, a+1)
        t = lowerBound' (\i -> tarr!i > q) (0, b+1)
        (sl, sr) = (sarr!(s-1), sarr!s)
        (tl, tr) = (tarr!(t-1), tarr!t)

-- lowerBound :: (Int -> Bool) -> (Int, Int) -> Int
lowerBound predicate bounds = go bounds
  where
    go !(!l, !h) | l + 1 == h  = h
                 | predicate m = m `seq` go (l, m)
                 | otherwise   = m `seq` go (m, h)
      where m = (l + h) `div` 2

lowerBound' predicate = last . unfoldr go 
  where
    go !(!l, !h) | l == h      = Nothing
                 | l + 1 == h  = Just (h, (h, h))
                 | predicate m = m `seq` Just (0, (l, m)) 
                 | otherwise   = m `seq` Just (0, (m, h))
      where m = (l + h) `div` 2