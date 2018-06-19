import Control.Monad

main :: IO()
main = do
  n <- readLn
  cs <- replicateM n $ fmap words getLine :: IO [[String]]
  putStrLn . unwords $ solve cs

solve :: [[String]] -> [String]
solve cs = foldl func [] cs
  where
    func acc (command:x)
      | command == "insert" = val:acc
      | command == "delete" = delete val acc
      | command == "deleteLast" = take (length acc - 1) acc
        where
          val = head x
          delete v (a:acc)
            | v == a = acc
            | otherwise = a:(delete v acc)
          delete _ acc = acc

    func (a:acc) (command:x)
      | command == "deleteFirst" = acc
        where val = head x
