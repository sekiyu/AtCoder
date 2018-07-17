
poland :: [String] -> String
poland xs = head $ foldl innerPoland [] xs

innerPoland :: [String] -> String -> [String]
innerPoland (a:b:c) x
  | x == "+"  = (add b a):c
  | x == "-"  = (sub b a):c
  | x == "*"  = (prod b a):c
  | otherwise = x:a:b:c
innerPoland acc x = (x:acc)

add :: String -> String -> String
add a b = show $ (read a :: Int) + (read b :: Int)

sub :: String -> String -> String
sub a b = show $ (read a :: Int) - (read b :: Int)

prod :: String -> String -> String
prod a b = show $ (read a :: Int) * (read b :: Int)
