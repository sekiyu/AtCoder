import Control.Monad
import qualified Data.Map.Strict as Map
--import qualified Data.HashMap.Strict as Map

main :: IO ()
main = do
  n <- readLn
  as <- getLine
  print $ solve n as

solve :: Int -> String -> Int
solve n as = foldr (\k acc -> acc + calc k) 0 $ Map.keys lptns
  where
    lptns = toMap . colorPatterns . take n $ as
    rptns = toMap . colorPatterns . reverse $ drop n as
    calc key = let l = Map.findWithDefault 0 key lptns
                   r = Map.findWithDefault 0 key rptns
               in l * r

colorPatterns :: [a] -> [([a], [a])]
colorPatterns xs = zip blues reds
  where
   n = length xs
   iss = filterM (\_ -> [True, False]) [0..(n - 1)]
   blues = map (\is -> [ xs !! i| i <- is]) iss
   reds = map (\is -> [ xs !! i| i <- (subt [0..(n - 1)] is) ]) iss

subt :: (Ord a) => [a] -> [a] -> [a]
subt as [] = as
subt [] bs = []
subt (a:as) (b:bs)
  | a == b = subt as bs
  | a < b  = a:(subt as (b:bs))
  | a > b  = subt (a:as) bs

toMap :: (Ord k) => [k] -> Map.Map k Int
toMap xs = Map.fromListWith (+) [(x, 1) | x <- xs]
