import Data.List



-- 二つのリストに含まれる共通要素の数を調べる
numCommonElements :: (Ord a) => [a] -> [a] -> Int
numCommonElements as bs = inner (sort as) (sort bs)
  where
    inner :: (Ord a) => [a] -> [a] -> Int
    inner [] _ = 0
    inner _ [] = 0
    inner (a:as) (b:bs)
      | a == b = 1 + inner as bs
      | a > b = inner (a:as) bs
      | a < b = inner as (b:bs)


-- 要素の置換
-- 先頭から調べて最初の要素だけ置換する
-- 見つからなかったときは何もしない
replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace from to (a:as)
  | from == a = to:as
  | otherwise = a:replace from to as


-- ⭐️特殊ソートの書き方
-- リストのリストをn番目の要素の大小でソート
sortOnN :: (Ord a) => Int -> [[a]] -> [[a]]
sortOnN n = sortOn (!! n)
sortByN :: (Ord a) => Int -> [[a]] -> [[a]]
sortByN n = sortBy (\x y -> compare (x!!n) (y!!n))
-- maximumBy, minimumBy
