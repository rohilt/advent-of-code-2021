import System.IO
import qualified Data.Map as Map
import Helper.Parse

type Point = (Int, Int)
type LineSegment = (Point, Point)

main :: IO()
main = do
  input <- readFile "in/day5"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [LineSegment]
parseInput = map parseLineSegment . lines
  where
    parseLineSegment = (\(a:_:b:[]) -> (parsePoint a, parsePoint b)) . words
    parsePoint = (\(x:y:[]) -> (x,y)) . splitNumsAtComma

part1 :: [LineSegment] -> Int
part1 = countIntersections . map (points False)

part2 :: [LineSegment] -> Int
part2 = countIntersections . map (points True)

countIntersections :: [[Point]] -> Int
countIntersections = length . filter (> 1) . Map.elems . foldl createMap Map.empty . concat

createMap :: Map.Map Point Int -> Point -> Map.Map Point Int
createMap map point = Map.insert point ((+) (Map.findWithDefault 0 point map) 1) map

points :: Bool -> LineSegment -> [Point]
points countDiagonals ((x1, y1), (x2, y2))
  | x1 == x2  = map (\y -> (x1, y)) $ range y1 y2
  | y1 == y2  = map (\x -> (x, y1)) $ range x1 x2
  | otherwise = if countDiagonals then zip (range2 x1 x2) (range2 y1 y2) else []

range :: Int -> Int -> [Int]
range x y
  | x < y     = [x..y]
  | otherwise = [y..x]

range2 :: Int -> Int -> [Int]
range2 x y
  | x < y     = [x..y]
  | otherwise = reverse $ [y..x]
