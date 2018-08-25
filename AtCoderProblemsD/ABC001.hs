import Control.Monad
import Data.List.Split
import Data.List

main :: IO ()
main = do
  n <- readLn
  -- am <- replicateM n $ map read . splitOn "-" <$> getLine :: IO [[Int]]
  am <- replicateM n getLine
  putStr . unlines . map toString . mergeIntersections . sort . toTimes $ am

toTimes :: [String] -> [(Int, Int)]
mergeIntersections :: [(Int, Int)] -> [(Int, Int)]

toTimes [] = []
toTimes (a:as) = t:toTimes as
  where
    (start:end:_) = map read . splitOn "-" $ a
    t = (s, e')
    s = start - start `mod` 5
    e = end + (5 - end `mod` 5) `mod` 5
    e' = if e `mod` 100 == 60
         then e + 40
         else e

mergeIntersections (a:[]) = [a]
mergeIntersections (a:b:c)
  | ae >= bs = mergeIntersections $ (as, e):c
  | otherwise = a:mergeIntersections (b:c)
  where
    (as, ae) = a
    (bs, be) = b
    e = max ae be

toString :: (Int, Int) -> String
toString (a, b) = (toTime a) ++ "-" ++ (toTime b)
  where
    toTime n
      | n < 10 = "000" ++ show n
      | n < 100 = "00" ++ show n
      | n < 1000 = "0" ++ show n
      | otherwise = show n
