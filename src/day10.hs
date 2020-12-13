import System.IO
import Data.List

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
    differences joltages = [ (sort joltages !! (i+1)) - (sort joltages !! i) | i <- [0..length joltages - 2] ]
    numOnes = length . filter (==1) . differences
    numThrees = length . filter (==3) . differences

part2 :: [Int] -> String
part2 _ = "Not yet implemented"
