import System.IO

type PreviousThreeDepths = (Int, Int, Int)
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
part2 = (\(_, c) -> c) . foldl countIncreasingDepthsBy3 ((0, 0, 0), -3)

countIncreasingDepthsBy3 :: (PreviousThreeDepths, Int) -> Int -> (PreviousThreeDepths, Int)
countIncreasingDepthsBy3 ((p1, p2, p3), count) c =
  ((p2, p3, c), if p1 < c then count + 1 else count)
