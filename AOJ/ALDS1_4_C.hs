{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Functor
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as B

main :: IO()
main = do
  n <- readLn :: IO Int
  ss <- B.getContents :: IO B.ByteString
  mapM_ putStrLn $ solve'' ss
  -- ss <- replicateM n $ (\(a:b:_) -> (a,b)) . words <$> getLine :: IO [(String, String)]
  -- mapM_ putStrLn $ solve' ss
  -- cs <- replicateM n $ fmap words getLine :: IO([[String]])
  -- solve cs

insertB = B.pack "insert"
findB = B.pack "find"

solve'' :: B.ByteString -> [String]
solve'' = reverse . snd . foldl' f (Set.empty, []) . B.lines
  where
    f :: (Set.Set B.ByteString, [String]) -> B.ByteString -> (Set.Set B.ByteString, [String])
    f !(dic, ss) !c
      | command == insertB = {-# SCC insert #-} let newDic = Set.insert key dic
                             in key `seq` newDic `seq` (newDic, ss)
      | command == findB = {-# SCC find #-} let val = if Set.member key dic then "yes" else "no"
                            in val `seq` (dic, val:ss)
      where (command:key:_) = B.words c
  

solve' :: [(String, String)] -> [String]
solve' = reverse . snd . foldl' f (Set.empty, [])
  where
    f :: (Set.Set String, [String]) -> (String, String) -> (Set.Set String, [String])
    f (dic, ss) (command, key)
      | command == "insert" = (Set.insert key dic, ss)
      | command == "find" = let val = if Set.member key dic then "yes" else "no"
                            in (dic, val:ss)

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
