import Data.List.Split
import Data.List


calcDoubleAndSlack [a1, a2, a3] = a1 * 3 + a2 * 2 + a3 * 2 -- all sides double, smallest triple for extra paper

calcSides [l, w, h] = calcDoubleAndSlack $ sort $ [l*w, w*h, h*l] -- sort to get smallest first

calcPackage x = calcSides $ map (read :: String -> Int) (splitOn "x" x)



calcCircumAndVolume [a1, a2, a3] = a1 * 2 + a2 * 2 + a1 * a2 * a3 -- small sides double + product of 3

calcRibbon x = calcCircumAndVolume $ sort $ map (read :: String -> Int) (splitOn "x" x)



main :: IO ()
main = do
  packages <- readFile "q2a.txt"
--  calcPaper (words packages) 0

  putStrLn $ show $ sum $ map calcPackage $ words packages

  putStrLn $ show $ sum $ map calcRibbon $ words packages


  -- putStrLn $ show $ splitOn "x" $


