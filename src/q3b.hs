import Data.List.Split
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set


followRoute route = followRoute1' route 0 0 0 0 (Set.fromList[(0, 0)])

followRoute1' [] _ _ _ _ places = places
followRoute1' ('>':rs) x1 y1 x2 y2 places = followRoute2' rs (x1 + 1) y1 x2 y2 (Set.insert (x1 + 1, y1) places)
followRoute1' ('<':rs) x1 y1 x2 y2 places = followRoute2' rs (x1 - 1) y1 x2 y2 (Set.insert (x1 - 1, y1) places)
followRoute1' ('^':rs) x1 y1 x2 y2 places = followRoute2' rs x1 (y1 + 1) x2 y2 (Set.insert (x1, y1 + 1) places)
followRoute1' ('v':rs) x1 y1 x2 y2 places = followRoute2' rs x1 (y1 - 1) x2 y2 (Set.insert (x1, y1 - 1) places)

followRoute2' [] _ _ _ _ places = places
followRoute2' ('>':rs) x1 y1 x2 y2 places = followRoute1' rs x1 y1 (x2 + 1) y2 (Set.insert (x2 + 1, y2) places)
followRoute2' ('<':rs) x1 y1 x2 y2 places = followRoute1' rs x1 y1 (x2 - 1) y2 (Set.insert (x2 - 1, y2) places)
followRoute2' ('^':rs) x1 y1 x2 y2 places = followRoute1' rs x1 y1 x2 (y2 + 1) (Set.insert (x2, y2 + 1) places)
followRoute2' ('v':rs) x1 y1 x2 y2 places = followRoute1' rs x1 y1 x2 (y2 - 1) (Set.insert (x2, y2 - 1) places)


main :: IO ()
main = do
  route <- readFile "q3a.txt"

  putStrLn $ show $ length $ followRoute route




