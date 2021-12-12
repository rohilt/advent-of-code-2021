import System.IO
import qualified Data.Set as Set
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
part1 = Set.size . Set.unions . points
  where
    points segments = [if l1 == l2 then Set.empty else (intersections l1 l2) | l1 <- segments, l2 <- segments]

intersections :: LineSegment -> LineSegment -> Set.Set Point
intersections ((x11, y11), (x12, y12)) ((x21, y21), (x22, y22))
  | x11 == x12 && x12 == x21 && x21 == x22  = Set.map createYPoint $ Set.intersection setY1 setY2
  | y11 == y12 && y12 == y21 && y21 == y22  = Set.map createXPoint $ Set.intersection setX1 setX2
  | x11 == x12 && y21 == y22                = if xySame then (Set.fromList [(x11, y21)]) else Set.empty
  | y11 == y12 && x21 == x22                = if yxSame then (Set.fromList [(x21, y11)]) else Set.empty
  | otherwise                               = Set.empty
    where
      createYPoint y = (x11, y)
      createXPoint x = (x, y11)
      setY1 = Set.fromList $ range y11 y12
      setY2 = Set.fromList $ range y21 y22
      setX1 = Set.fromList $ range x11 x12
      setX2 = Set.fromList $ range x21 x22
      xySame = elem x11 setX2 && elem y21 setY1
      yxSame = elem y11 setY2 && elem x21 setX1
-- intersections ((x1, y11), (x1, y12)) ((x21, y2), (x22, y2)) =
-- intersections ((x11, y1), (x12, y1)) ((x2, y21), (x2, y22)) =
-- intersections _ _ = Set.empty

range :: Int -> Int -> [Int]
range x y
  | x < y     = [x..y]
  | otherwise = [y..x]

part2 :: [LineSegment] -> Int
part2 _ = 0
