import Data.List

enoughVowels :: Int -> String -> Int
enoughVowels count [] = count
enoughVowels count (c:cs) = if elem c "aeiou" then enoughVowels (count + 1) cs else enoughVowels count cs

twoInARow :: Maybe Char -> String -> Bool
twoInARow _ [] = False
twoInARow Nothing (x:xs) = twoInARow (Just x) xs
twoInARow (Just y) (x:xs) = if x == y then True else twoInARow (Just x) xs

-- ab, cd, pq, or xy
notIllegal :: String -> Bool
notIllegal x = notIllegal' x ["ab", "cd", "pq", "xy"]

notIllegal' :: String -> [String] -> Bool
notIllegal' _ [] = True
notIllegal' x (i:is) = if isInfixOf i x then False else notIllegal' x is

countValid :: Int -> [String] -> Int
countValid n [] = n
countValid n (x:xs) = if (enoughVowels 0 x) >= 3 && twoInARow Nothing x && notIllegal x then countValid (n+1) xs else countValid n xs

main :: IO ()
main = do

  strings <-readFile "q5a.txt"

  putStrLn $ show $ countValid 0 (words strings)



