import Debug.Trace
main :: IO ()
main = do
  readLn >>= putStrLn . unwords . map show . solve' 

solve' :: Integer -> [Integer]
solve' s
  | s == 10^18 = [0,0,10^9,0, 0, 10^9]
  | otherwise = [0, 0, 10^9, 1, x, y] 
  where
    check = area [0, 0, 10^9, 1, x, y] 
    (b, a) = quotRem s (10^9)
    x = 10^9 - a
    y = b + 1
    -- area = abs (10^9 * y - x)
    area [x, y, ax, ay, bx, by] = abs $ (ax-x)*(by-y) - (ay-y)*(bx-x)
