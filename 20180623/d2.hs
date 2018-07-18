import Data.List

main :: IO()
main = do
  k <- readLn :: IO Int
  let candidates = map read [solve n | n <- [0..10*15]] :: [Integer]
      snks = map (\n -> (snkf n, n)) candidates
  printsnk k snks

printsnk :: Int -> [(Double, Integer)] -> IO()
printsnk 0 _ = return ()
printsnk n (s:nk) = do
  let (x, y) = s
      (xm, ym) = minimum nk
  if y < ym then print y else return ()
  printsnk (n - 1) nk

solve n = (show (s + 1)) ++ replicate d '9'
  where
    d = div n 9
    s = mod n 9

f :: String -> Integer -> Integer
f [] n = n
f (s:ss) n = f ss (read [s] + n)

snkf :: Integer -> Double
snkf v = (fromIntegral v) / (fromIntegral sn)
  where
    vstr = show v
    sn = f vstr 0
