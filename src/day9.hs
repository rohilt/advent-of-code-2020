import System.IO
import Data.List
import Data.Maybe

main :: IO()
main = do
  input <- readFile "../input/day9.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: [Int] -> Int
part1 xs = fromJust $ head $ filter isJust [if (elem (xs !! i) (prevSum i)) then Nothing else Just (xs !! i) | i <- [25..(length xs - 1)] ]
  where
    prevSum i = [x + y | x <- take 25 $ drop (i-25) xs, y <- take 25 $ drop (i-25) xs]

part2 :: [Int] -> String
part2 _ = "Not yet implemented"
