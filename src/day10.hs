import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map

main :: IO()
main = do
  input <- readFile "../input/day10.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input
  print $ differences $ parseInput input

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
countNumWays (x:y:xs)
  | x + y <= 3 = countNumWays ((x+y):xs) + countNumWays (y:xs)
  | otherwise = countNumWays (y:xs)
countNumWays _ = 1
