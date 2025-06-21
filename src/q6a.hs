-- https://bluebones.net/2007/01/replace-in-haskell/
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


-- might help: https://hackage.haskell.org/package/range-0.3.0.2/docs/Data-Range.html

main :: IO ()
main = do

  lights <-readFile "q6a.txt"

  putStrLn $ show $ wordsWhen (=='\n') (replace (replace lights "turn " "") "through " "")



