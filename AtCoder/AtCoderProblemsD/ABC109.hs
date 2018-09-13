import Control.Monad

main :: IO ()
main = do
  (h:w:_) <- fmap (map read . words) getLine :: IO [Int]
  am <- replicateM h $ map read . words <$> getLine :: IO [[Int]]
  let bam = map (map even) am
  let ans = (solve 1 bam) ++ (vSolve w bam)
  print $ length ans
  forM_ (map (unwords . map show) ans) putStrLn

-- solve : 右への移動だけで出来るだけ奇数を消す
-- 消し切れなかった奇数は右端へ移動
-- vSolve : 右移動だけで消し切れなかった奇数を縦移動で消す

solve :: Int -> [[Bool]] -> [[Int]]
solve _ [] = []
solve i (as:am) = (manip i 1 as) ++ (solve (i + 1) am)
  where
    manip :: Int -> Int -> [Bool] -> [[Int]]
    manip _ _ (a:[]) = []
    manip i j (a:as)
      | a = manip i (j + 1) as
      | otherwise = [i, j, i, j + 1] : (manip i (j + 1) $ move as)

move :: [Bool] -> [Bool]
move [] = []
move (a:as) = (not a):as

vSolve :: Int -> [[Bool]] -> [[Int]]
vSolve w am = manip 1 $ map (even . length . filter (\x -> not x)) $ am
  where
    manip _ (b:[]) = []
    manip i (b:bs)
      | b = manip (i + 1) bs
      | otherwise = [i, w, i + 1, w] : (manip (i + 1) (move bs))
