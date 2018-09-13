import Data.List

main :: IO()
main = do
  k <- readLn :: IO Int
--  putStr . unlines $ take k [show n | n <- [1..], snk n]
--  putStr . unlines $ take k [show n ++ ", " ++ show (n `div` (f (show n) 0)) | n <- [1..]]
  let candidates = map read [solve n | n <- [0..9*15]] :: [Integer]
      snks = map (\n -> (snkf n, n)) candidates
  printsnk k snks
--  putStr . unlines . map show . take k $ sort [solve n | n <- [0..9*15]]

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

--solve2 k c:cs =

snk :: Integer -> Bool
--snk k = last (show k) == '9' || k < 10
snk k = solve (drop 1 (show k)) || k < 10
  where
    solve :: String -> Bool
    solve "" = True
    solve (s:ss) = s == '9' && solve ss

f :: String -> Integer -> Integer
f [] n = n
f (s:ss) n = f ss (read [s] + n)

snkf :: Integer -> Double
snkf v = (fromIntegral v) / (fromIntegral sn)
  where
    vstr = show v
    sn = f vstr 0
