import System.IO

data Instruction = Forward Int | Down Int | Up Int

main :: IO()
main = do
  input <- readFile "in/day2"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Instruction]
parseInput = map (formatInstruction . words) . lines
  where
    formatInstruction ("forward":x:[]) = Forward (read x :: Int)
    formatInstruction ("down":x:[]) = Down (read x :: Int)
    formatInstruction ("up":x:[]) = Up (read x :: Int)

part1 :: [Instruction] -> Int
part1 = (\(h, d) -> h * d) . foldl updatePosition (0, 0)
  where
    updatePosition (h, d) (Forward x) = (h + x, d)
    updatePosition (h, d) (Up x) = (h, d - x)
    updatePosition (h, d) (Down x) = (h, d + x)

part2 :: [Instruction] -> Int
part2 = (\(h, d, _) -> h * d) . foldl updatePosition (0, 0, 0)
  where
    updatePosition (h, d, a) (Forward x) = (h + x, d + (a * x), a)
    updatePosition (h, d, a) (Up x) = (h, d, a - x)
    updatePosition (h, d, a) (Down x) = (h, d, a + x)
