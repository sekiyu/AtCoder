{-# LANGUAGE BangPatterns #-}
import Control.Monad
import qualified Data.ByteString.Char8  as B
import qualified Data.IntMap.Strict     as IntMap

main :: IO ()
main = do
  n <- readLn
  -- xs <- replicateM (2 * n) getLine :: IO [String]
  -- solve xs

  xs <- replicateM (2 * n) B.getLine :: IO [B.ByteString]
  solveBs xs

solveBs :: [B.ByteString] -> IO ()
solveBs [] = return ()
solveBs (x:y:z) = do
  -- print $ lcs' (B.unpack x) (B.unpack y) []
  print $ lcs'2 (B.unpack x) (B.unpack y)
  solveBs z

solve :: [String] -> IO ()
solve [] = return ()
solve (x:y:z) = do
--  print $ lcs x y 0
  print $ lcs' x y []
  solve z

lcs :: String -> String -> Int -> Int
lcs xs [] v = v
lcs [] ys v = v
lcs (x:xs) (y:ys) v
  | x == y = lcs xs ys (v + 1)
  | otherwise = max (lcs xs (y:ys) v) (lcs (x:xs) ys v)


lcs' :: String -> String -> [Int] -> Int
lcs' !x !y !table
  | i >= m * n = head table
  | x !! j == y !! k = lu `seq` lcs' x y $! lu:table
  | otherwise = let !maxlu = (max l u)
                in lcs' x y $! maxlu:table
  where
    m = length x
    n = length y
    i = length table
    j = mod i m
    k = div i m
    l = if j > 0 then head table else 0
    u = if k > 0 then table !! (m - 1) else 0
    lu = 1 + if j > 0 && k > 0 then table !! m else 0

lcs'2 :: String -> String -> Int
lcs'2 !x !y = lcs x y m n 0 IntMap.empty
  where
    m = length x
    n = length y
    lcs !x !y !m !n i table
      | i >= m * n = table IntMap.! (i - 1)
      | x !! j == y !! k = let !ii = i + 1
                           in lu `seq` lcs x y m n ii $! IntMap.insert i lu table
      | otherwise = let !maxlu = (max l u)
                    in let !ii = (i + 1)
                    in lcs x y m n ii $! IntMap.insert i maxlu table
        where
          j = mod i m
          k = div i m
          l = if j > 0 then table IntMap.! (i - 1) else 0
          u = if k > 0 then table IntMap.! (i - m) else 0
          lu = 1 + if j > 0 && k > 0 then table IntMap.! (i - m - 1) else 0
