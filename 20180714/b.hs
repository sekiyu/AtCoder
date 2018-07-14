import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)

readInts = map (fst . fromJust . BC.readInt) . BC.words <$> BC.getLine

main :: IO ()
main = do
  n <- readLn
  --qs <- replicateM n $ fmap (map read . words) getLine :: IO [[Integer]]
  qs <- replicateM n readInts :: IO [[Int]]
  let ans = map solve qs
  seq ans $ mapM_ (\x -> putStrLn $ if x then "Yes" else "No") ans

solve :: [Int] -> Bool
solve as
  | a < b || b > d = False
  | b == d = a `mod` b <= c
  | c >= b = True
  | otherwise = ow
  where
    (a:b:c:d:_) = as
    gcdbd = gcd b d
    ow = all (\x -> (x + if x <= c then d else 0) >= b) $ [(a + x * gcdbd) `mod` b| x <- [0..(div b gcdbd)]]

gcd' :: (Integral a) => [a] -> a
gcd' [] = 1
gcd' [x] = x
gcd' (x:xs) = gcd x (gcd' xs)
