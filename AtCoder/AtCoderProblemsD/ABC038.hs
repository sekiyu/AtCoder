import Control.Monad
import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  n <- readLn
  am <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  -- print . naive . map toBox $ am
  print . solve . map toBox $ am

solve :: [Box] -> Int
solve boxes = dp (sort boxes) Map.empty
  where
    dp :: [Box] -> Map.Map Box Int -> Int
    dp [] dps = maximum . Map.elems $ dps
    dp (b:bs) dps = dp bs newdps
      where
        insides = Map.keys dps
        newdps = dps


naive :: [Box] -> Int
naive boxes = maximum . map (pack []) . permutations $ boxes

type Box = (Int, Int)
toBox :: [Int] -> Box
toBox (a:b:_) = (a, b)

pack :: [Box] -> [Box] -> Int
pack [] (b:bs) = pack [b] bs
pack as [] = length as
pack (a:as) (b:bs) = if a `canBeIn` b
                     then pack (b:a:as) bs
                     else pack (a:as) bs

canBeIn :: Box -> Box -> Bool
a `canBeIn` b = aw < bw && ah < bh
  where
    (aw, ah) = a
    (bw, bh) = b
