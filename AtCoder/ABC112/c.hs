-- ABC112 C - Pyramid
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  am <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  putStrLn . unwords . map show $ solve am

solve :: [[Int]] -> [Int]
solve am = head . filter isOk $ [[x,y,h] | (x,y) <- candidates, h <- heightCandidates]
  where
    maxHeight = maximum . map (!!2) $ am
    heightCandidates = [(maxHeight - 200)..(maxHeight + 200)]
    isOk (cx:cy:ch:_) = all (\(x:y:h:_) -> height ch (x,y) (cx,cy) == h) am

type Position = (Int, Int)
height :: Int -> Position -> Position -> Int
height h (x, y) (cx, cy) = max (h - (abs (x - cx)) - (abs (y - cy))) 0

candidates = [ (x,y)| x <- [0..100], y <- [0..100]]
