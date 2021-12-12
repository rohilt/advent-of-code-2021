import System.IO
import qualified Data.Map as Map
import Helper.Parse

type Fish = Map.Map Int Int

main :: IO()
main = do
  input <- readFile "in/day6"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> Fish
parseInput = foldl addToMap Map.empty . splitNumsAtComma
  where
    addToMap map value = Map.insert value ((+) (Map.findWithDefault 0 value map) 1) map

part1 :: Fish -> Int
part1 = sum . Map.elems . head . drop 80 . iterate iterateDay

part2 :: Fish -> Int
part2 = sum . Map.elems . head . drop 256 . iterate iterateDay

iterateDay :: Fish -> Fish
iterateDay map = Map.insert 8 newFish $ Map.mapKeysWith (+) updateKey map
  where
    newFish = Map.findWithDefault 0 0 map
    updateKey k = if k == 0 then 6 else k-1
