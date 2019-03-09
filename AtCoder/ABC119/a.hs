-- ABC119 A
main :: IO ()
main = do
  s <- getLine
  putStrLn $ if isHeisei s then "Heisei" else "TBD"

isHeisei :: String -> Bool
isHeisei s = y * 10000 + m * 100 + d <= 20190430
  where
    y = read $ take 4 s
    m = read $ take 2 $ drop 5 s
    d = read $ drop 8 s
