-- ARC006 C - 積み重ね
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  ws <- replicateM n readLn :: IO [Int]
  print $ solve ws

solve :: [Int] -> Int
solve ws = putBox ws []
  where
    putBox :: [Int] -> [Int] -> Int
    putBox [] onFloors = length onFloors
    putBox (w:ws) onFloors
      | null puttables = putBox ws (w:onFloors)
      | otherwise = putBox ws new
      where
        puttables = filter (w <=) onFloors
        toPut = minimum puttables
        new = replace toPut w onFloors

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace from to (a:as)
  | from == a = to:as
  | otherwise = a:replace from to as
