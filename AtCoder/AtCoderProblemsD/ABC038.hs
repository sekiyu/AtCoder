import Control.Monad
import qualified Data.Map.Strict as Map
import Data.List
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

main :: IO ()
main = do
  n <- readLn
  am <- replicateM n $ readInts
  -- print . naive . map toBox $ am
  print . solve . map toBox $ am

type Box = (Int, Int)
toBox :: [Int] -> Box
toBox (a:b:_) = (a, b)

solve :: [Box] -> Int
solve boxes = dp (sort boxes) Map.empty
  where
    dp :: [Box] -> Map.Map Box Int -> Int
    dp [] dps = maximum . Map.elems $ dps
    dp (b:bs) dps = dp bs newdps
      where
        smallers = filter (`canBeIn` b) $ Map.keys dps
        maxdp = maximum . map (dps Map.!) $ smallers
        newdps = case smallers of
          [] -> Map.insert b 1 dps
          _  -> Map.insert b (1 + maxdp) dps



canBeIn :: Box -> Box -> Bool
a `canBeIn` b = aw < bw && ah < bh
  where
    (aw, ah) = a
    (bw, bh) = b

-- 全探索で解く
naive :: [Box] -> Int
naive boxes = maximum . map (pack []) . permutations $ boxes

pack :: [Box] -> [Box] -> Int
pack [] (b:bs) = pack [b] bs
pack as [] = length as
pack (a:as) (b:bs) = if a `canBeIn` b
                     then pack (b:a:as) bs
                     else pack (a:as) bs
