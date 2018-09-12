import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ if n < 1200 then "ABC"
             else (if n < 2800 then "ARC" else "AGC")
