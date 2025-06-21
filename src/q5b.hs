import Data.List

repeatWithOneBetween :: Maybe Char -> Maybe Char -> String -> Bool
repeatWithOneBetween _ _ [] = False
repeatWithOneBetween (Just a) b (x:xs) = if a == x then True else repeatWithOneBetween b (Just x) xs
repeatWithOneBetween Nothing b (x:xs) = repeatWithOneBetween b (Just x) xs

hasDoublePair :: Maybe Char -> String -> Bool
hasDoublePair _ [] = False
hasDoublePair Nothing (x: xs) = hasDoublePair (Just x) xs
hasDoublePair (Just a) (x: xs) = if isInfixOf [a, x] xs then True else hasDoublePair (Just x) xs

countValid :: Int -> [String] -> Int
countValid n [] = n
countValid n (x:xs) = if repeatWithOneBetween Nothing Nothing x && hasDoublePair Nothing x then countValid (n+1) xs else countValid n xs

main :: IO ()
main = do
  strings <-readFile "q5a.txt"
  putStrLn $ show $ countValid 0 (words strings)





