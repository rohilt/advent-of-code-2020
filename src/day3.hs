import System.IO
import Data.List

main :: IO()
main = do
  input <- readFile "../input/day3.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [[Square]]
parseInput input = map parseLine $ lines input
  where parseLine = map (\x -> if (x == '.') then Open else Tree)

part1 :: [[Square]] -> Int
part1 grid = countTrees grid 3

part2 :: [[Square]] -> Int
part2 grid = product $ ((countTrees' grid 2):(map (countTrees grid) [1,3,5,7]))

countTrees :: [[Square]] -> Int -> Int
countTrees grid slope = length $ filter isTree squares
  where
    indices = map (\x -> mod x $ length (grid !! 0) ) [0,slope..(slope*(length grid)-1)]
    squares = [(grid !! i) !! (indices !! i) | i <- [0..(length grid - 1)]]

countTrees' :: [[Square]] -> Int -> Int
countTrees' grid slope = length $ filter isTree squares
  where
    indices = map (\x -> mod x $ length (grid !! 0) ) [0..]
    indices' = [0,slope..(length grid - 1)]
    squares = [(grid !! (indices' !! i) ) !! (indices !! i) | i <- [0..((length indices') - 1)]]


data Square = Open | Tree

isTree :: Square -> Bool
isTree Tree = True
isTree Open = False
