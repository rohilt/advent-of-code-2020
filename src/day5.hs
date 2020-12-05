import System.IO
import Data.List

main :: IO()
main = do
  input <- readFile "input/day5.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [String]
parseInput input = lines input

part1 :: [String] -> Int
part1 seats = maximum $ map computeSeatNumber seats

part2 :: [String] -> Int
part2 seats = head $ filter (\x -> (elem (x - 1) seatsFound) && (elem (x + 1) seatsFound)) seatsLeft
  where
    seatsFound = map computeSeatNumber seats
    seatsLeft = [0..1023] \\ seatsFound

computeSeatNumber :: String -> Int
computeSeatNumber s = 8*(computeSeatRow $ take 7 s) + (computeSeatColumn $ drop 7 s)

computeSeatRow :: String -> Int
computeSeatRow row = sum [if (row !! (6-i) == 'B') then (2^i) else 0| i <- [0..6]]

computeSeatColumn :: String -> Int
computeSeatColumn column = sum [if (column !! (2-i) == 'R') then (2^i) else 0| i <- [0..2]]
