main :: IO()
main = do
  k <- readLn :: IO Int
--  putStr . unlines $ take k [show n | n <- [1..], snk n]
--  putStr . unlines $ take k [show n ++ ", " ++ show (n `div` (f (show n) 0)) | n <- [1..]]
  putStr . unlines $ take k [solve n | n <- [0..]]

solve n = (show (s + 1)) ++ replicate d '9'
  where
    d = div n 9
    s = mod n 9

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
