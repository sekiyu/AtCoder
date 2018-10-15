-- TDPC C - トーナメント
import Control.Monad
import Data.Maybe
import Debug.Trace
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

main :: IO ()
main = do
  k <- readLn :: IO Int
  rs <- replicateM (2^k) readLn :: IO [Int]
  let dic = Map.fromListWith (+) . join $ solve k rs
  forM_ rs (\r -> print $ dic Map.! r)

type Prob = (Int, Double)

prob :: Prob -> Prob -> [Prob]
prob (p, pp) (q, qp)
  = let e = 1 / (1 + 10**(fromIntegral (q - p) / 400))
    in [(p, pp * qp * e), (q, pp * qp * (1 - e))]

solve k rs = foldr (.) id (replicate k proceed) $ initial
  where
    initial = map (:[]) . zip rs $ repeat 1 :: [[Prob]]

proceed :: [[Prob]] -> [[Prob]]
proceed [] = []
proceed (ps:qs:probs) = join [prob p q | p <- ps, q <- qs] : proceed probs
