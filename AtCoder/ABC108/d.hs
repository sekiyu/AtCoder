import Control.Monad

main :: IO ()
main = do
  l <- readLn
  solve' l

solve :: Int -> IO ()
solve l = do
  let n = numOfNodes l
      ans = foldr (\i acc -> acc ++ edgesAt i n) [] [0..l]
  forM_ ans (putStrLn . unwords . map show)

patterns n = 2^(n - 2)
-- numOfNodes l = head . filter (\n -> patterns n >= l) $ [3..]
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


numOfNodes = (1+) . ceiling . logBase 2 . fromIntegral

solve' :: Int -> IO ()
solve' l = do
  putStrLn . unwords . map show $ [n, m]
  forM_ edges (putStrLn . unwords . map show)
  where
    n = numOfNodes l
    edges = (zeroEdges n) ++ (biEdges l n)
    m = length edges
    zeroEdges :: Int -> [[Int]]
    zeroEdges 1 = []
    zeroEdges k = [(k-1), k, 0]:zeroEdges (k-1)
    biEdges :: Int -> Int -> [[Int]]
    biEdges k n = go 1 k n
      where
        go :: Int -> Int -> Int -> [[Int]]
        go i k n
          | 2^i >= k = [(k - 2^(i-1)),n,2^(i-1)]:[]
          | otherwise = [i, i + 1, 2^(i- 1)]:go (i + 1) k n
