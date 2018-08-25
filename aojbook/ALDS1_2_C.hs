{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Functor

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

isSameCard :: Trump -> Trump -> Bool
Card a x `isSameCard` Card b y = a == b && x == y

main :: IO ()
main = do
  _ <- getLine
  as <- toTrump . words <$> getLine :: IO [Trump]
  let (bubbly, n) = bubbleSort as
  let merged = sort as
  putStrLn . unwords . map show $ bubbly
  putStrLn (if show bubbly == show merged
            then "Stable"
            else "Not stable")
  let selection = selectionSort as
  putStrLn . unwords . map show $ selection
  putStrLn (if show selection == show merged
            then "Stable"
            else "Not stable")

bubbleSort :: (Ord a) => [a] -> ([a], Int)
bubbleSort [] = ([], 0)
bubbleSort as = cons' (x, i) $ bubbleSort $ xs
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

-- selectionSort :: (Ord a) => [a] -> [a]
selectionSort (a:b:[]) = if a > b then [b, a] else [a, b]
selectionSort (a:as)
  | a <= m = a:selectionSort as
  | a > m = m:selectionSort rep
  where
    m = minimum as
    rep = replaceFirst m a as

-- replaceFirst :: (Eq a) => a -> a -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst old new (l:ls)
  | isSameCard old l = new:ls
  | otherwise = l:replaceFirst old new ls
