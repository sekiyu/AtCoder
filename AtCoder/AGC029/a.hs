-- AGC029 A
main :: IO ()
main = getLine >>= print . solve . toBools

solve :: [Bool] -> Int
solve ss
  | all (not . id) ss = 0
  | otherwise = length a + solve (a ++ b')
    where
      (a, b) = span (not . id) . dropWhile id $ ss
      b' = if null b then [] else (tail b)

toBools [] = []
toBools (s:ss) = (if s == 'W'
                 then True
                 else False):toBools ss
