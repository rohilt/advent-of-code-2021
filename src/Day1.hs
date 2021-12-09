import System.IO

main :: IO()
main = do
  input <- readFile "in/day1"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput input = map (\x -> read x :: Int) $ lines input

part1 :: [Int] -> Int
part1 = (\(_, c) -> c) . foldl countIncreasingDepths (0, -1)

countIncreasingDepths :: (Int, Int) -> Int -> (Int, Int)
countIncreasingDepths (previousDepth, count) currentDepth =
  (currentDepth, if previousDepth < currentDepth then count + 1 else count)

part2 :: [Int] -> Int
part2 _ = 0
