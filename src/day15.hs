import System.IO
import Data.List
import qualified Data.Map as Map

type GameMemory = Map.Map Int Int
type Turn = (Int, Int, GameMemory)

main :: IO()
main = do
  input <- readFile "../input/day15.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput = splitAtComma . init

splitAtComma :: String -> [Int]
splitAtComma input
  | (not $ elem ',' input) = [read input]
  | otherwise = (read $ takeWhile (/= ',') input):(splitAtComma $ tail $ dropWhile (/= ',') input)

part1 :: [Int] -> Int
part1 nums = ((init nums) ++ (map (\(x,_,_) -> x) $ iterate numSpokenAt (last nums, length nums + 1, generateInitialMemory $ init nums))) !! (2020-1)

part2 :: [Int] -> String
part2 _ = "Not yet implemented"

generateInitialMemory :: [Int] -> GameMemory
generateInitialMemory xs = Map.fromList $ zip xs [1..]

numSpokenAt :: Turn -> Turn
numSpokenAt (prevNum, turn, mem)
  | Map.member prevNum mem = (turn - 1 - (mem Map.! prevNum), turn+1, newMem)
  | otherwise = (0, turn+1, newMem)
  where
    newMem = Map.insert prevNum (turn-1) mem

-- numSpokenAt :: Int -> [Int] -> GameMemory -> Int
-- numSpokenAt n nums m
  -- | length nums == n = head nums --reverse input, place new elements at front
