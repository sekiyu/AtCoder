import Data.List
import qualified Data.IntMap.Strict as IntMap

-- IntMap.lookupMaxがAtCoderになかったので自分で実装
lookupMax :: IntMap.IntMap Int -> (Int, Int)
lookupMax = last . sortOn snd . IntMap.toList
