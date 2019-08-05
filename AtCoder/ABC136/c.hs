{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
readInt = fst . fromJust . B.readInt
readInts = map readInt . B.words <$> B.getLine :: IO [Int]

main :: IO ()
main = do
  n <- readLn :: IO Int
  hs <- readInts :: IO [Int]
  putStrLn . solve $ hs

solve :: [Int] -> String
solve hs = if isNothing $ foldM f 0 hs
           then "No"
           else "Yes"
  where
    f !prev !x = if prev == x
               then Just x
               else if prev <= x - 1
                    then Just (x - 1)
                    else Nothing