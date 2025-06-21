

import Data.Char
import Data.Digits (digits, unDigits)

-- https://stackoverflow.com/questions/10028213/converting-number-base
convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

convertPasswordToNum :: String -> [Int]
convertPasswordToNum [] = []
convertPasswordToNum (x:xs) = (ord x - ord 'a'):convertPasswordToNum xs

convertNumToPassword :: [Int] -> String
convertNumToPassword [] = []
convertNumToPassword (x:xs) = chr(x + ord 'a'):convertNumToPassword xs

addOne :: [Int] -> [Int]
addOne [] = [1]
addOne (x:xs) = reverse $ addOne' (reverse (x:xs)) 1

addOne' [] 0 = []
addOne' [] 1 = [1] -- should not happen
addOne' (x:xs) 0 = x:addOne' xs 0
addOne' (x:xs) 1 = if (x + 1) > 25 then 0:addOne' xs 1 else (nextX x):addOne' xs 0

nextX x
  | x == ord 'l' = x + 2
  | x == ord 'o' = x + 2
  | x == ord 'i' = x + 2
  | otherwise = x + 1


inceasing3 [] = False
inceasing3 (x:xs) = inceasing3' xs x 1

inceasing3' [] _ _ = False
inceasing3' (x:xs) lastX n = if x == lastX + 1 then if n == 3 then True else inceasing3' xs x (n+1) else inceasing3' xs x 1

overlapping [] = False
overlapping (x:xs) = overlapping1' xs x

overlapping1' [] _ = False
overlapping1' (x:xs) y = if x == y then overlapping2' xs x y else overlapping1' xs x

overlapping2' [] _ _ = False
overlapping2' (x:xs) y z = if x == y && x /= z then True else overlapping2' xs x z

findNext pw = if inceasing3 pw && overlapping pw then pw else findNext $ addOne pw

main :: IO ()
main = do

  let currentPassword = "vzbxkghb" -- vzccdeff niet goed?

  putStrLn $ show $ convertNumToPassword $ findNext $ convertPasswordToNum currentPassword


