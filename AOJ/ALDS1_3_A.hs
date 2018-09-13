
main :: IO()
main = do
  ws <-  getLine :: IO String
  print . head . foldl rpn [] $ words ws

rpn :: [Int] -> String -> [Int]
rpn (x:y:zs) "+" = (x + y):zs
rpn (x:y:zs) "-" = (y - x):zs
rpn (x:y:zs) "*" = (x * y):zs
rpn acc w = (read w):acc
