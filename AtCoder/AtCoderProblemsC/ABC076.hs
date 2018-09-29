-- ABC076 C - Dubious Document 2
main :: IO ()
main = do
  s' <- getLine
  t <- getLine
  putStrLn $ solve s' t

solve :: String -> String -> String
solve s' t = let ans = filter (isConvertible s') $ candidates s' t
             in if null ans
                then "UNRESTORABLE"
                else toAns . last $ ans


toAns [] = []
toAns (s:ss)
  | s == '?' = 'a':toAns ss
  | otherwise = s:toAns ss

isConvertible :: String -> String -> Bool
isConvertible s candidate = all (\(x,y) -> x == y || x == '?') $ zip s candidate

candidates :: String -> String -> [String]
candidates s' t = encode s' t 0
encode :: String -> String -> Int -> [String]
encode s' t i
  | slength < tlength + i = []
  | otherwise = (top ++ t ++ bottom):encode s' t (i+1)
  where
    slength = length s'
    tlength = length t
    (top, middle) = splitAt i s'
    (_, bottom) = splitAt tlength middle
