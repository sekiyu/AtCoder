{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

main :: IO ()
main = do
  _ <- getLine
  -- as <- fmap (map read . words) getLine :: IO [Int]
  as <- readInts
  let (sorted, n) = solveStrictly as -- bubbleSortC as
  putStrLn . unwords . map show . reverse $ sorted
  print n
  -- solve as

data StList a = Nil | !a :! !(StList a) deriving (Show)
-- toStrict :: (Foldable t) => t a -> StList a
toStrict   = foldr (:!) Nil
-- fromStrict = foldr (:) []
fromStrict Nil = []
fromStrict (s :! ss) = s:fromStrict ss
fromList = toStrict
toList   = fromStrict

solveStrictly :: (Ord a, Show a) => [a] -> ([a], Int)
solveStrictly as = bubbleSort Nil (toStrict as) 0
  where
    -- bubbleSort :: (Ord a, Show a) => [a] -> [a] -> Int -> ([a], Int)
    bubbleSort sorted Nil n = (fromStrict sorted, n)
    bubbleSort Nil (b :! bs) i = bubbleSort (b :! Nil) bs i
    bubbleSort (a :! as) (b :! bs) i
      | a < b     = bubbleSort (b :! (a :! as)) bs i
      | otherwise = bubbleSort as (b :! (a :! bs)) $! (i + 1)

solve :: (Ord a, Show a) => [a] -> ([a], Int)
solve as = bubbleSort [] as 0
  where
    bubbleSort :: (Ord a, Show a) => [a] -> [a] -> Int -> ([a], Int)
    bubbleSort sorted [] n = (sorted, n)
    -- do
      -- putStrLn . unwords . map show . reverse $ sorted
      -- print n
    bubbleSort [] (b:bs) i = bubbleSort [b] bs i
    bubbleSort (a:as) (b:bs) i
      | a < b     = bubbleSort (b:a:as) bs i
      | otherwise = bubbleSort as (b:a:bs) $! (i + 1)

bubblesort :: (Ord a) => [a] -> [a]
bubblesort [] = []
bubblesort xs = x:bubblesort ys
    where
        x:ys = foldr bubble [] xs
        bubble :: (Ord a) => a -> [a] -> [a]
        bubble p [] = [p]
        bubble p (q:ps)
            | q < p     = q:p:ps
            | otherwise = p:q:ps

bubbleSort' :: (Ord a) => [a] -> [a]
bubbleSort' as = reverse . bubble $ reverse as
  where
    bubble :: (Ord a) => [a] -> [a]
    bubble [x] = [x]
    bubble (a:b:as)
      | a < b     = b:(bubble $ a:as)
      | otherwise = a:(bubble $ b:as)

bubbleSortC :: (Ord a) => [a] -> ([a], Int)
bubbleSortC [] = ([], 0)
bubbleSortC as = cons' (x, i) $ bubbleSortC $ xs
  where
    ((x:xs), i) = foldr bubble ([], 0) as

    bubble :: (Ord a) => a -> ([a], Int) -> ([a], Int)
    bubble x ([], i) = ([x], i)
    bubble x ((a:as), i)
      | a < x     = (a:x:as, i + 1)
      | otherwise = (x:a:as, i)

    cons' :: (Ord a) => (a, Int) -> ([a], Int) -> ([a], Int)
    cons' a b = let (x, i) = a
                    (xs, j) = b
                in (x:xs, i + j)
