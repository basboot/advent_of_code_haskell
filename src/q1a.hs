elevator :: [Char] -> Int -> Int
elevator [] floor = floor
elevator (x:xs) floor = if x == '(' then elevator xs (floor + 1) else elevator xs (floor - 1)


main :: IO ()
main = do
  contents <- readFile "q1a.txt"
  putStrLn contents
  putStrLn $ show $ elevator contents 0

