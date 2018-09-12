{-# OPTIONS_GHC -O #-}

import Control.Monad
import Data.List
import Data.Char

main=do
    k<-readLn :: IO Int
    forM_ (take k $ chk $ [1..9] ++ [19,29..99] ++ [n*10^k+(10^k-1) | k<-[0..12], n<-[100..999]]) print

chk [] = []
chk (n:ns) = case find (\v -> (s n) > (s v)) ns of
                Just _ -> chk ns
                Nothing -> n:(chk ns)

s a = (fromIntegral a) / (fromIntegral $ sum $ map digitToInt $ show a)
