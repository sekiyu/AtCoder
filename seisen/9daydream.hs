import Data.List

main = do
  s <- getLine
  putStrLn $ if solve (reverse s) then "YES" else "NO"

solve :: [Char] -> Bool
solve [] = True
solve ('m':'a':'e':'r':'d':s) = solve s
solve ('r':'e':'m':'a':'e':'r':'d':s) = solve s
solve ('r':'e':'s':'a':'r':'e':s) = solve s
solve ('e':'s':'a':'r':'e':s) = solve s
solve _ = False
