-- ABC079 C Train Ticket
import Data.Char
import Control.Monad

main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve s = showFormula digits cleared
  where
    digits = map digitToInt s
    candidates = replicateM 3 ["+", "-"]
    cleared = head $ filter (\c -> doOps c digits == 7) candidates
    showFormula :: [Int] -> [String] -> String
    showFormula [d] [] = show d ++ "=7"
    showFormula (d:ds) (c:cs) = show d ++ c ++ showFormula ds cs

doOps :: (Num a) => [String] -> [a] -> a
doOps _ [a] = a
doOps [] (a:as) = a
doOps (f:fs) (a:b:as) = doOps fs (op f a b:as)
  where
    op "+" a b = a + b
    op "-" a b = a - b
