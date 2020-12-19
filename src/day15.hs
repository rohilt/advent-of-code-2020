import System.IO
import Data.List
import qualified Data.Map as Map
import Helper.Parse

type GameMemory = Map.Map Int Int
type Turn = (Int, Int, GameMemory)

main :: IO()
main = do
  input <- readFile "../input/day15.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput = splitNumsAtComma . init

part1 :: [Int] -> Int
part1 = getNthNumber 2020

part2 :: [Int] -> Int
part2 = getNthNumber 30000000

generateInitialMemory :: [Int] -> GameMemory
generateInitialMemory xs = Map.fromList $ zip xs [1..]

numSpokenAt :: Turn -> Turn
numSpokenAt (prevNum, turn, mem) = if Map.member prevNum mem then (turn - 1 - (mem Map.! prevNum), turn+1, newMem) else (0, turn+1, newMem)
  where
    newMem = Map.insert prevNum (turn-1) mem

getNthNumber :: Int -> [Int] -> Int
getNthNumber n nums = (\(x,_,_) -> x) $ (iterate numSpokenAt (last nums, length nums + 1, generateInitialMemory $ init nums)) !! (n - (length nums))
