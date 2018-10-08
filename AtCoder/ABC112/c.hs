-- ABC112 C - Pyramid
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  am <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  putStrLn . unwords . map show $ solve am

solve :: [[Int]] -> [Int]
solve am = head . filter isOk $ [[x,y,centerH (x,y)] | x <- [0..100], y <- [0..100]]
  where
    isOk (cx:cy:ch:_) = all (\(x:y:h:_) -> height ch (x,y) (cx,cy) == h) am
    highest = last $ sortBy (\a b -> compare (a!!2) (b!!2)) am
    centerH = calcBackHeight highest

type Position = (Int, Int)
height :: Int -> Position -> Position -> Int
height h (x, y) (cx, cy) = max (h - (abs (x - cx)) - (abs (y - cy))) 0
calcBackHeight :: [Int] -> Position -> Int
calcBackHeight a (cx, cy) = h + (abs (x - cx)) + (abs (y - cy))
  where
    x = a !! 0
    y = a !! 1
    h = a !! 2
