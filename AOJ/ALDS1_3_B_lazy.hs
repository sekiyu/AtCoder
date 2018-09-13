import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)

main :: IO()
main = do
  (n:q:_) <- fmap (map read . words) getLine :: IO[Int]
  ps <- replicateM n $ BC.words <$> BC.getLine
  mapM_ (BC.putStrLn . BC.unwords)  $ solve ps q 0

solve :: [[BC.ByteString]] -> Int -> Int -> [[BC.ByteString]]
solve [] _ _ = []
solve (p:ps) q elapsed
  | v <= q = [w, BC.pack $ show finished]:(solve ps q finished)
  | v > q  = solve (ps ++ [[w, BC.pack $ show $ v - q]]) q $ elapsed + q
  where
    [w, vbytestr] = p
    v = fst . fromJust $ BC.readInt vbytestr :: Int
    finished = elapsed + v
