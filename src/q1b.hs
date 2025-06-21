elevator :: [Char] -> Int -> Int -> Int
elevator _ (-1) n = n
elevator [] floor _ = floor
elevator (x:xs) floor n = if x == '(' then elevator xs (floor + 1) (n + 1) else elevator xs (floor - 1) (n + 1)


main :: IO ()
main = do
  contents <- readFile "q1a.txt"
  putStrLn contents
  putStrLn $ show $ elevator contents 0 0

