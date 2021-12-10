import System.IO
import Data.Array
import Data.Char

main :: IO()
main = do
  input <- readFile "in/day3"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt . reverse) . lines

part1 :: [[Int]] -> Int
part1 = getProduct . countBits

countBits :: [[Int]] -> ([Int], Int)
countBits numbers = (foldl (zipWith (+)) [0,0..] numbers, length numbers)

getProduct :: ([Int], Int) -> Int
getProduct (counts, count) = gamma * epsilon
  where
    compareTo = div count 2
    gamma = toBinary $ map (\b -> if b > compareTo then 1 else 0) counts
    epsilon = toBinary $ map (\b -> if b < compareTo then 1 else 0) counts

toBinary :: [Int] -> Int
toBinary = (\(_, x) -> x) . foldl handleBinary (0, 0)
  where
    handleBinary (i, num) bit = (i+1, ((2^i)*bit) + num)

part2 :: [[Int]] -> Int
part2 _ = 0
