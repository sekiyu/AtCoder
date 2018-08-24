{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.List
import Data.Functor

readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

data Trump = Card Int Char
instance Show Trump where
  show (Card n c) = c:(show n)
instance Eq Trump where
  Card a _ == Card b _ = a == b
instance Ord Trump where
  Card a _ `compare` Card b _ = a `compare` b

toTrump :: [String] -> [Trump]
toTrump [] = []
toTrump ((a:as):ass) = (Card (read as) a):toTrump ass

main :: IO ()
main = do
  _ <- getLine
  as <- toTrump . words <$> getLine :: IO [Trump]
  let (bubbly, n) = bubbleSortC as
  let merged = sort as
  putStrLn . unwords . map show $ bubbly
  putStrLn (if bubbly == merged
            then "Stable"
            else "Not stable")


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
