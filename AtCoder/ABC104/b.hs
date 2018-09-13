import Control.Monad

main :: IO ()
main = do
  s <- getLine
  putStrLn $ if solve s then "AC" else "WA"

solve :: String -> Bool
solve s = foldr1 (&&) conds
  where
    n = length s
    conds = [
      head s == 'A',
      length (filter (== 'C') (drop 2 . take (n - 1) $ s)) == 1,
      length [x | x<- s, x `elem` ['a'..'z']] == n - 2]
