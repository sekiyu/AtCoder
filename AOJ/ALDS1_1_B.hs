import Data.Functor

main :: IO ()
main = do
  (x:y:_) <- map read . words <$> getLine :: IO [Int]
  print $ gcd' x y

gcdGreed :: Int -> Int -> Int
gcdGreed x y = head [z | z <- reverse [1..(min x y)], mod x z == 0, mod y z == 0]

gcd' :: Int -> Int -> Int
gcd' x y = if x `mod` y == 0
           then y
           else gcd' y $! x `mod` y
