import System.IO

main :: IO()
main = do
  input <- readFile "in/day6"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> Int
parseInput _ = 0

part1 :: Int -> Int
part1 _ = 0

part2 :: Int -> Int
part2 _ = 0
