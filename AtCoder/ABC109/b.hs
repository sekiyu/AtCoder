-- ABC109 B
import Control.Monad
import qualified Data.Set as Set

main :: IO ()
main = do
  n <- readLn
  ws <- replicateM n $ getLine :: IO [String]
  putStrLn $ solve ws


solve :: [String] -> String
solve (w:ws) = go w ws $ Set.singleton w
  where 
    go :: String -> [String] -> Set.Set String -> String
    go _ [] _ = "Yes"
    go prev (w:ws) hist 
      | Set.member w hist = "No"
      | otherwise = if last prev == head w 
                    then go w ws (Set.insert w hist)
                    else "No"
        