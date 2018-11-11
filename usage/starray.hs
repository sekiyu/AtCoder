import Control.Monad.ST
import Data.Array.ST
import Data.Array.IO
import Data.Array

makeIOUArray :: IO (IOUArray Int Bool)
makeIOUArray = do
    arr <- newArray (1,100) True    -- 配列の作成
    val <- readArray arr 50         -- 要素の値を取得
    writeArray arr 55 (not val)     -- 要素の値を変更
    return arr                      -- 配列をモナドに包む

makeSTArray :: ST s (STArray s Int Bool)
makeSTArray = do
    arr <- newArray (1,100) True    -- 配列の作成
    val <- readArray arr 50         -- 要素の値を取得
    writeArray arr 55 (not val)     -- 要素の値を変更
    return arr                      -- 配列をモナドに包む


makeSTUArray :: ST s (STUArray s Int Bool)
makeSTUArray = do
    arr <- newArray (1,100) True    -- 配列の作成
    val <- readArray arr 50         -- 要素の値を取得
    writeArray arr 55 (not val)     -- 要素の値を変更
    return arr                      -- 配列をモナドに包む

showSTArray = print $ runSTArray makeSTArray
showSTUArray = print $ runSTUArray makeSTUArray

test = (runSTArray arr)!50
  where
    arr = newArray (1,100) maxBound :: ST s (STArray s Int Int)
    dp = foldl (\)
