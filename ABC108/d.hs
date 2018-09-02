import Control.Monad

main :: IO ()
main = do
  l <- readLn
  solve l

solve :: Int -> IO ()
solve l = do
  let n = numOfNodes l
      ans = foldr (\i acc -> acc ++ edgesAt i n) [] [0..l]
  forM_ ans (putStrLn . unwords . map show)

patterns n = 2^(n - 2)
numOfNodes l = head . filter (\n -> patterns n >= l) $ [3..]
node = ceiling . logBase 2 . fromIntegral

edgesAt :: Int -> Int -> [[Int]]
edgesAt 0 n = [[1, n, 0]]
edgesAt i n
  | i == n = [[n - 2, n - 1, nodei]]
  | elem i $ map (2^) [0..nodeNext]
      = [[1, ith, 2^(ith - 2)], [ith, n, 0]]
  | otherwise = []
  where
    nodei = (node i)
    nodeNext = node (i + 1)
    ith = nodeNext + 1
    noden = node n
