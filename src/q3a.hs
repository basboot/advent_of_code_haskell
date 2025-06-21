import Data.List.Split
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set


followRoute route = followRoute' route 0 0 (Set.fromList[(0, 0)])

followRoute' [] _ _ places = places
followRoute' ('>':rs) x y places = followRoute' rs (x + 1) y (Set.insert (x + 1, y) places)
followRoute' ('<':rs) x y places = followRoute' rs (x - 1) y (Set.insert (x - 1, y) places)
followRoute' ('^':rs) x y places = followRoute' rs x (y + 1) (Set.insert (x, y + 1) places)
followRoute' ('v':rs) x y places = followRoute' rs x (y - 1) (Set.insert (x, y - 1) places)


main :: IO ()
main = do
  route <- readFile "q3a.txt"

  putStrLn $ show $ length $ followRoute route




