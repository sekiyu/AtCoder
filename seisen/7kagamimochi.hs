import Data.List
import Control.Monad

main :: IO()
main = do
  n <- getLine
  ds <- replicateM (read n) readLn :: IO [Int]
  print . length $ nub ds
