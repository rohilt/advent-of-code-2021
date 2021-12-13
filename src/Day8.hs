import System.IO

type Display = ([String], [String])

main :: IO()
main = do
  input <- readFile "in/day8"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Display]
parseInput = map parseLine . lines
  where
    parseLine = parseDisplay . words
    parseDisplay (s1:s2:s3:s4:s5:s6:s7:s8:s9:s10:"|":digits) = ([s1,s2,s3,s4,s5,s6,s7,s8,s9,s10], digits)

part1 :: [Display] -> Int
part1 = foldl countDigits 0
  where
    countDigits count (_, digits) = (+) count $ length $ filter (is1478 . length) digits
    is1478 x = any (== x) [2,3,4,7]

part2 :: [Display] -> Int
part2 _ = 0
