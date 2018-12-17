-- AGC029 A
main :: IO ()
main = getLine >>= print . solve . toBools

solve :: [Bool] -> Int
solve ss = go 0 ss
  where 
    go _ [] = 0
    go i (s:ss)
      | s == False = go (i+1) ss
      | otherwise = i + go i ss

toBools [] = []
toBools (s:ss) = (if s == 'W'
                 then True
                 else False):toBools ss

solve' :: [Bool] -> Int
solve' ss
  | all (not . id) ss = 0
  | otherwise = length a + solve (a ++ b')
    where
      (a, b) = span (not . id) . dropWhile id $ ss
      b' = if null b then [] else (tail b)

                 