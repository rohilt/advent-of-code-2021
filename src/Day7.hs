import System.IO
import Data.List
import Helper.Parse

main :: IO()
main = do
  input <- readFile "in/day7"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput = splitNumsAtComma

part1 :: [Int] -> Int
part1 list = sum $ map fuelCost list
  where
    align = median $ sort list
    fuelCost position = abs (position - align)

median :: [Int] -> Int
median list
  | mod (length list) 2 == 1 = (list !! middle)
  | mod (length list) 2 == 0 = div ((+) (list !! middle) (list !! middle2)) 2
    where
      middle = div (length list) 2
      middle2 = (-) (div (length list) 2) 1

part2 :: [Int] -> Int
part2 list = min (sum $ map (fuelCost align1) list) (sum $ map (fuelCost align2) list)
  where
    align = average list
    align1 = floor align
    align2 = ceiling align
    fuelCost align position = div ((*) (abs (position - align)) ((+ 1) $ abs (position - align))) 2

average :: [Int] -> Double
average list = (fromIntegral $ sum list) / (fromIntegral $ length list)
