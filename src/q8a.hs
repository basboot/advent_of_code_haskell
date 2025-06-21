main :: IO ()

stringMem [] = 0
stringMem ('\\':'x':_:_:xs) = 1 + stringMem xs
stringMem ('\\':'\\':xs) = 1 + stringMem xs
stringMem ('\\':'"':xs) = 1 + stringMem xs
stringMem ('"':xs) = 0 + stringMem xs
stringMem (_:xs) = 1 + stringMem xs

literalMem [] = 2 -- for ""
literalMem ('\\':xs) = 2 + literalMem xs
literalMem ('"':xs) = 2 + literalMem xs
literalMem (_:xs) = 1 + literalMem xs


main = do

  strings <-readFile "q8a.txt"

  putStrLn $ show $ (foldr (\x y -> y + length x) 0 (words strings) - foldr (\x y -> y + stringMem x) 0 (words strings))

  putStrLn $ show $ (foldr (\x y -> y + literalMem x) 0 (words strings) - foldr (\x y -> y + length x) 0 (words strings))


