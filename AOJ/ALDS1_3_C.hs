{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.List
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Debug.Trace
import Data.Functor
import qualified Data.Sequence as S
import Data.Foldable

main :: IO()
main = do
  n <- readLn :: IO Int
  -- cs <- replicateM n $ fmap words getLine :: IO [[String]]
  -- cs <- replicateM n B.getLine :: IO [B.ByteString]
  -- putStrLn . unwords . map show $ solve cs
  cs <- B.getContents :: IO B.ByteString
  B.putStrLn . B.unwords $ solve'' cs

insertC = B.pack "insert"
deleteC = B.pack "delete"
deleteLastC = B.pack "deleteLast"
deleteFirstC = B.pack "deleteFirst"

solve'' :: B.ByteString -> [B.ByteString]
solve'' = toList . foldl' func S.empty . B.lines
  where
    func :: S.Seq B.ByteString -> B.ByteString -> S.Seq B.ByteString
    func acc cs
      | command == insertC = val S.:<| acc
      | command == deleteLastC = S.deleteAt (S.length acc - 1) acc
      | command == deleteFirstC = S.deleteAt 0 acc
      | command == deleteC = delete val acc
        where
          (command:c) = B.words cs
          (val:_) = c
          delete !v seq = left S.>< (S.deleteAt 0 right)
            where
              (left, right) = S.breakl (==v) seq

solve' :: B.ByteString -> [B.ByteString]
solve' = foldl' func [] . B.lines
  where
    func :: [B.ByteString] -> B.ByteString -> [B.ByteString]
    func acc cs
      -- | command == insertC = val `seq` val:acc
      | command == insertC = val:acc
      | command == deleteC = delete val acc
      | command == deleteLastC = init acc
      | command == deleteFirstC = tail acc
      -- | otherwise = traceShow command []
        where
          (command:c) = B.words cs
          -- val = fst . fromJust . B.readInt . head $ c
          -- val = head $ c
          (val:_) = c
          delete _ [] = []
          delete !v (b:bcc) = if v == b
                              then bcc
                              else b:(delete v bcc)
  
solve :: [[String]] -> [String]
solve = foldl' func []
  where
    func acc (command:c)
      | command == "insert" = val `seq` val:acc
      | command == "delete" = delete val acc
      | command == "deleteLast" = init acc
      | command == "deleteFirst" = tail acc
        where
          val = head c
          delete _ [] = []
          delete !v (b:bcc) = if v == b
                              then bcc
                              else b:(delete v bcc)

{-
deleteFirstElem :: (Eq a) => a -> [a] -> [a]
deleteFirstElem v xs = myscanl' f [] xs
  where
    f :: a -> a -> a
    f _ x | x == v = []

myscanl' :: (b -> a -> b) -> b -> [a] -> [b]
myscanl' = scanlGo'
  where
    scanlGo'           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo' f !q ls    = q : (case ls of
                            []   -> []
                            x:xs -> scanlGo' f (f q x) xs)
            
-}