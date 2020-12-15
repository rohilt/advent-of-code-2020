import System.IO
import Data.List

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

part1 :: [Int] -> [Int]
part1 i = i

part2 :: [Int] -> String
part2 _ = "Not yet implemented"
