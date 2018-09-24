-- ABC110 C String Transformation
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  putStrLn $ if solve s t
             then "Yes"
             else "No"

solve :: String -> String -> Bool
solve s t = go s t (Map.fromList $ zip s t) (Map.fromList $ zip t s)
  where
    go :: String -> String -> Map.Map Char Char -> Map.Map Char Char -> Bool
    go [] _ _ _ = True
    go _ [] _ _ = True
    go (s:ss) (t:ts) smp tmp
       = smp Map.! s == t && tmp Map.! t == s
       && go ss ts smp tmp


{-
      | Map.member s mp = mp Map.! s == t && go ss ts mp
      | Map.member t mp = mp Map.! t == s && go ss ts mp
      | otherwise = go ss ts new
      where
        new = Map.insert s t mp
        new2 = Map.insert t s mp
-}
