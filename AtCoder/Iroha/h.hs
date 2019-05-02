import Data.Char
import Debug.Trace

main :: IO ()
main = getLine >>= putStrLn . solve 

solve s = if (read s :: Integer) == (read mn)
          then next
          else if head mn == '0' then tail mn else mn
  where
    m = f s `mod` 9
    n = f s `div` 9
    f = sum . map (read . (:[]))
    mn = if read s < 10 
         then '1':show (read s - 1)
         else (intToDigit m):replicate n '9'
    next = if m == 9
           then "18" ++ (replicate (n-1) '9')
           else (intToDigit $ m+1):'8':replicate (n-1) '9'
