import Data.List
import Control.Monad
import qualified Data.Map as Map

main :: IO()
main = do
  n <- readLn
  cs <- replicateM n $ fmap words getLine :: IO([[String]])
  solve cs

solve :: [[String]] -> IO()
solve cs = innerSolve cs Map.empty
  where
--    innerSolve :: [[String]] -> Map.Map -> IO()
    innerSolve [] _ = return ()
    innerSolve (c:cs) m
      | d == "insert" = innerSolve cs $ Map.insert w 1 m
      | d == "find" = do
        if Map.member w m
          then putStrLn "yes"
          else putStrLn "no"
        innerSolve cs m
      | otherwise = putStrLn "error"
        where (d:w:_) = c
