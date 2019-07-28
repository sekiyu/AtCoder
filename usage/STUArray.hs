import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef

bf = "+++++++++[>++++++++<-]>."

main = do
    let jmp = runST $ do
        jmp <- newArray (0, length bf - 1) 0 :: ST s (STUArray s Int Int)
        loops <- newSTRef []
        forM_ [0 .. length bf - 1] $ \i -> do
            case bf !! i of
                '[' -> modifySTRef loops (i:)
                ']' -> do
                    start <- do
                        (h:t) <- readSTRef loops
                        writeSTRef loops t
                        return h
                    writeArray jmp start i
                    writeArray jmp i start
                _ -> return ()
        getElems jmp
    print jmp