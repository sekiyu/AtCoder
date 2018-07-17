
binarySearch :: (Ord a) => a -> [a] -> Int
binarySearch a xs
  | n == 1          = if (head xs) == a then 0 else -1
  | xs !! mid  == a = mid
  | xs !! mid  > a  = binarySearch a $ take mid xs
  | xs !! mid  < a  = (mid +) . binarySearch a . reverse
    . take (if (mod n 2 == 0) then mid + 1 else mid) $ reverse xs
  where
    n = length xs
    mid = n `div` 2


fBinarySearch :: (a -> a -> Ordering) -> a -> [a] -> Int
fBinarySearch f a xs
  | n == 1                  = if f (head xs) a == EQ then 0 else -1
  | f (xs !! mid) a == EQ   = mid
  | f (xs !! mid) a == LT   = fBinarySearch f a $ take mid xs
  | f (xs !! mid) a == GT   = (mid +) . fBinarySearch f a . reverse
    . take (if (mod n 2 == 0) then mid + 1 else mid) $ reverse xs
  where
    n = length xs
    mid = n `div` 2


sample :: IO()
sample = print $ fBinarySearch f a as
  where
    a = (3, "asahina")
    as = [(1, "yuki"), (2, "haruhi"), (3, "mikuru"), (4, "ikki")]
    f x y = compare (fst x) (fst y)
