-- ABC119 B
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  xus <- replicateM n $ (\(a:b:_) -> (read a,b)) . words <$> getLine :: IO [(Double, String)]

  print $ solve xus

btcjpy = 380000

solve = foldr f 0 
  where
    f (x, u) acc = let ujpy = if u == "JPY" then 1 else btcjpy
                   in acc + x * ujpy