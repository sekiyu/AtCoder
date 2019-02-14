{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Array.Unboxed
import Data.Functor
import Data.List
import qualified Data.ByteString.Char8  as B
import qualified Data.IntMap.Strict     as IntMap
import qualified Data.Vector.Unboxed    as VU

main :: IO ()
main = do
  n <- readLn
  -- replicateM n iterateSolveDp
  -- return ()
  --xs <- replicateM (2 * n) getLine :: IO [String]
  -- solveDp xs
  -- xs <- replicateM (2 * n) B.getLine :: IO [B.ByteString]
  -- solveBs xs
  xs <- replicateM n get2Lines :: IO [(String, String)]
  -- mapM_ (print . uncurry dpLCS) xs -- メモリ制限に引っかかっている
  mapM_ (print . uncurry rowdp) xs -- メモリ制限にはかからないがTLE

get2Lines :: IO (String, String)
get2Lines = do
  a <- getLine
  b <- getLine
  return (a, b)

rowdp :: String -> String -> Int
rowdp xs ys = (VU.foldl' scanY headRow xv)VU.!m
  where
    n = length xs
    m = length ys
    xv = VU.fromList xs :: VU.Vector Char
    headRow = VU.replicate (m + 1) 0 :: VU.Vector Int
    scanY :: VU.Vector Int -> Char -> VU.Vector Int
    scanY prevRow x = VU.scanl' f 0 $ VU.fromList $ zip ys [1..]
      where
        f :: Int -> (Char, Int) -> Int
        f v (y, i) 
          | x == y = 1 + prevRow VU.! (i-1)
          | otherwise = max (prevRow VU.! i) v

    -- fはyのi文字目とxを比較してDPのi列目を作る
    -- f :: VU.Vector Int -> Char -> VU.Vector
    

dpLCS :: String -> String -> Int
dpLCS _ [] = 0
dpLCS a b =
  let nextRow ac prevRow =
        let diagonals = 0:prevRow
            lefts = 0:thisRow
            ups = prevRow
            maxes = zipWith max lefts ups
            thisRow = zipWith3 (\diag maxLeftUp bc ->
                                    if bc == ac then 1 + diag else maxLeftUp)
                                    diagonals maxes b
        in thisRow

      firstRow = map (\_ -> 0) b
      dpTable = firstRow:zipWith nextRow a dpTable
  in last (last dpTable)
  
mydp :: String -> String -> Int
mydp x y = dp!(n, m)
  where
    n = length x
    m = length y
    a = listArray (0, n-1) x :: Array Int Char
    b = listArray (0, m-1) y :: Array Int Char
    bounds = ((0, 0), (n, m)) -- dpをn x mの行列で行なっていることがメモリ不足の原因？
    dp :: Array (Int, Int) Int
    dp = listArray bounds [f ij | ij <- range bounds]
    f (0, _) = 0
    f (_, 0) = 0
    f (i, j)
      | a!(i-1) == b!(j-1) = 1 + dp!(i-1, j-1)
      | otherwise = max (dp!(i, j-1)) (dp!(i-1, j))


solveBs :: [B.ByteString] -> IO ()
solveBs [] = return ()
solveBs (x:y:z) = do
  -- print $ lcs' (B.unpack x) (B.unpack y) []
  -- print $ lcs'2 (B.unpack x) (B.unpack y)
  print $ dpLCS (B.unpack x) (B.unpack y)
  solveBs z
  

iterateSolveDp = do
  x <- B.unpack <$> B.getLine
  y <- B.unpack <$> B.getLine
  let n = length x
      m = length y
      a = listArray (0, n-1) x :: Array Int Char
      b = listArray (0, m-1) y :: Array Int Char
      dp :: Array (Int, Int) Int
      dp = array ((0, 0), (n, m)) $ [((i, j), f (i, j)) |i<-[0..n],j<-[0..m]]
      f (0, _) = 0
      f (_, 0) = 0
      f (i, j)
        | a!(i-1) == b!(j-1) = 1 + dp!(i-1, j-1)
        | otherwise = max (dp!(i, j-1)) (dp!(i-1, j))
  print $ dp!(n, m)

  

solveDp :: [String] -> IO ()
solveDp [] = return ()
solveDp (x:y:z) = do
  let n = length x
      m = length y
      a = listArray (0, n-1) x :: Array Int Char
      b = listArray (0, m-1) y :: Array Int Char
      dp :: Array (Int, Int) Int
      dp = listArray ((0, 0), (n, m)) $ map f [(i, j)|i<-[0..n],j<-[0..m]]
      f (0, _) = 0
      f (_, 0) = 0
      f (i, j)
        | a!(i-1) == b!(j-1) = 1 + dp!(i-1, j-1)
        | otherwise = max (dp!(i, j-1)) (dp!(i-1, j))
  print $ dp!(n, m)
  solveDp z



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
    !m = length x
    !n = length y
    lcs !x !y !m !n i !table
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
