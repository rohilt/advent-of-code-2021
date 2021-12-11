import System.IO
import Data.List
import qualified Data.Set as Set
import Helper.Parse

type BingoContext = (BingoNumbers, [BingoCard])
type BingoSetup = ((BingoNumbers, BingoNumbers), [BingoCardPossibilities])
type BingoNumbers = [Int]
type BingoCard = [[Int]]
type BingoCardPossibilities = [Set.Set Int]

main :: IO()
main = do
  input <- readFile "in/day4"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> BingoContext
parseInput = parseBingoContext . splitOn "" . lines
  where
    parseBingoContext ([nums]:cards) = (splitNumsAtComma nums, parseBingoCards cards)
    parseBingoCards [] = []
    parseBingoCards (card:cards) = (map (map (\x -> read x :: Int) . words) card):(parseBingoCards cards)

part1 :: BingoContext -> Int
part1 = getWinnerScore . head . dropWhile noWinners . iterate playBingo . setupBingo

setupBingo :: BingoContext -> BingoSetup
setupBingo (nums, cards) = (([], nums), map generateCardSets cards)
  where
    generateCardSets card = (++) (map Set.fromList card) (map Set.fromList $ transpose card)

playBingo :: BingoSetup -> BingoSetup
playBingo ((called, num:nums), cards) = ((num:called, nums), map (map (removeNumber num)) cards)
  where
    removeNumber x = Set.filter (/= x)

noWinners :: BingoSetup -> Bool
noWinners (_, cards) = not $ any (any ((==) 0 . Set.size)) cards

getWinnerScore :: BingoSetup -> Int
getWinnerScore ((lastCalled:_, _), cards) = (*) lastCalled $ getProduct cards
  where
    getProduct = sum . Set.unions . head . filter (any ((==) 0 . Set.size))

part2 :: BingoContext -> Int
part2 = getWinnerScore . head . dropWhile noLastWinner . iterate playBingo2 . setupBingo

playBingo2 :: BingoSetup -> BingoSetup
playBingo2 ((called, num:nums), cards) = ((num:called, nums), map (map (removeNumber num)) $ filter alreadyWon cards)
  where
    removeNumber x = Set.filter (/= x)
    alreadyWon = not . any ((==) 0 . Set.size)

noLastWinner :: BingoSetup -> Bool
noLastWinner (_, card:[]) = not $ any ((==) 0 . Set.size) card
noLastWinner _ = True
