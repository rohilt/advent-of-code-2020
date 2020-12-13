import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map

main :: IO()
main = do
  input <- readFile "../input/day10.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: [Int] -> Int
part1 joltages = ((+1) $ numOnes joltages) * ((+1) $ numThrees joltages)
  where
    numOnes = length . filter (==1) . differences
    numThrees = length . filter (==3) . differences

part2 :: [Int] -> Int
part2 = countNumWays . differences . (0:)

differences :: [Int] -> [Int]
differences joltages = [ (sort joltages !! (i+1)) - (sort joltages !! i) | i <- [0..length joltages - 2] ]

countNumWays :: [Int] -> Int
countNumWays i = fst $ countNumWays' i Map.empty

countNumWays' :: [Int] -> Map.Map [Int] Int -> (Int, Map.Map [Int] Int)
countNumWays' (x:y:xs) m
  | isJust (Map.lookup (x:y:xs) m) = (fromJust (Map.lookup (x:y:xs) m), m)
  | x + y <= 3 = (fst sol1 + fst sol2, newMap)
  | otherwise = (fst sol2, newMap)
    where
      sol1 = if (x + y <= 3) then countNumWays' ((x+y):xs) m else (0, m)
      sol2 = countNumWays' (y:xs) (snd sol1)
      newMap = Map.insert (y:xs) (fst sol2) (snd sol2)
countNumWays' _ m = (1,m)
