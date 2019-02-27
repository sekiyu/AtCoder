

-- 条件を満たす最初の要素を指すインデックスを返す
-- lowerBound :: (Ord a, Ix a) => (a -> Bool) -> (a, a) -> a
lowerBound :: (Int -> Bool) -> (Int, Int) -> Int
lowerBound predicate bounds = go bounds
  where
    go (l, h) | l + 1 == h = h
              | predicate m = go (l, m)
              | otherwise = go (m, h)
      where m = (l + h) `div` 2

-- unfoldr版
-- 上の方が速いらしい
lowerBound' predicate = last . unfoldr go 
  where
    go !(!l, !h)  | l == h      = Nothing
                  | l + 1 == h  = Just (h, (h, h))
                  | predicate m = m `seq` Just (0, (l, m)) 
                  | otherwise   = m `seq` Just (0, (m, h))
      where m = (l + h) `div` 2

-- predicate x == a を満たす要素xのインデックスを返す
binarySearch :: (Ord a) => (Int -> a) -> a -> (Int, Int) -> Int
binarySearch predicate a bounds = go bounds
  where
    go (l, h) | predicate m == a = m
              | predicate m < a  = go (l, m)
              | otherwise        = go (m, h)
      where m = (l + h) `div` 2