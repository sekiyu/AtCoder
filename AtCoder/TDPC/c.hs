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
  let rWithId = zip [1..] rs
  let dic = Map.fromListWith (+) . join $ solve k rWithId
  forM_ rWithId (\r -> print $ dic Map.! r)

solve k rs = foldr (.) id (replicate k (aggregate . proceed)) $ initial
  where
    initial = map (:[]) . zip rs $ repeat 1 :: [[Prob]]

type Id = Int
type Rating = Int
type Prob = ((Id, Rating), Double)

prob :: Prob -> Prob -> [Prob]
prob ((i, p), pp) ((j, q), qp)
  = let e = 1 / (1 + 10**(fromIntegral (q - p) / 400))
    in [((i, p), pp * qp * e), ((j, q), pp * qp * (1 - e))]

proceed :: [[Prob]] -> [[Prob]]
proceed [] = []
proceed (ps:qs:probs) = join [prob p q | p <- ps, q <- qs] : proceed probs

aggregate :: [[Prob]] -> [[Prob]]
aggregate = map (Map.toList . Map.fromListWith (+))
