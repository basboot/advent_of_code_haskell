import Data.Hash.MD5

checkHash :: String -> Bool
-- Part A only 5 zeros checkHash ('0':'0':'0':'0':'0':xs) = True
checkHash ('0':'0':'0':'0':'0':'0':xs) = True
checkHash _ = False


findNonce :: String -> Int -> Int
findNonce input nonce = if checkHash $ (md5s (Str (input ++ (show nonce)))) then nonce else findNonce input (nonce + 1)

main :: IO ()
main = do
  let input = "iwrupvqb"

  putStrLn $ show $ findNonce input 0




