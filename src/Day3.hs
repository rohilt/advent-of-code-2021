import System.IO
import Data.Array
import Data.Char

main :: IO()
main = do
  input <- readFile "in/day3"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

part1 :: [[Int]] -> Int
part1 = getProduct . countBits . (map reverse)

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
part2 numbers = (oxygen numbers) * (co2 numbers)

oxygen :: [[Int]] -> Int
oxygen ([]:_) = 0
oxygen numbers = firstBitDecimal + (oxygen $ map tail $ filter (\l -> head l == firstBitValue) numbers)
  where
    firstBit = (fromIntegral (sum $ map head numbers)) >= ((fromIntegral (length numbers)) / (fromIntegral 2))
    firstBitValue = if firstBit then 1 else 0
    firstBitDecimal = if firstBit then (2 ^ (length (head numbers) - 1)) else 0

co2 :: [[Int]] -> Int
co2 ([]:_) = 0
co2 numbers = firstBitDecimal + (co2 $ map tail $ filter (\l -> head l == firstBitValue) numbers)
  where
    firstBit = if length numbers == 1 then
      (head $ head numbers) == 1
    else
      ((fromIntegral (sum $ map head numbers)) < ((fromIntegral (length numbers)) / (fromIntegral 2)))
    firstBitValue = if firstBit then 1 else 0
    firstBitDecimal = if firstBit then (2 ^ (length (head numbers) - 1)) else 0
