import Control.Monad

main :: IO ()
main = do
  (d:g:_) <- fmap (map read . words) getLine :: IO [Int]
  pcs <- replicateM d $ map read . words <$> getLine :: IO [[Int]]
  print $ eval [3, 1] pcs
  print $ compPoints pcs
  -- print $ solve g pcs

-- solve :: Int -> [[Int]] -> Int
-- solve g pcs =
-- where
-- maxComp = maximum [(i * 100 * (head p) + last p) >= g | (i, p) <- zip [1..] pcs]


compPoints :: [[Int]] -> [[Int]]
compPoints pcs = zipWith (\ps i -> let num = ps !! 0
                                   in [i, num, num * i * 100 + ps !! 1]) pcs [1..]

candidates' n = replicateM n [0, 1]


candidates :: [Int] -> [[Int]]
candidates [] = []
candidates (p:ps) = [x | x <- [0..p] ]:candidates ps

problems :: [[Int]] -> [Int]
problems [] = []
problems (p:ps) = (head p):problems ps

eval :: [Int] -> [[Int]] -> Int
eval solvedList pcs = innerEval solvedList pcs [1..]
  where
    innerEval [] _ _ = 0
    innerEval _ [] _ = 0
    innerEval (s:ss) (p:ps) (i:is)
      = i * s * 100
        + (if (s == head p) then (last p) else 0)
        + innerEval ss ps is
