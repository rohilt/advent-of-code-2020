import System.IO
import Data.List
import qualified Data.Set as Set

main :: IO()
main = do
  input <- readFile "input/day6.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [[String]]
parseInput input = splitGroups $ lines input
  where
    splitGroups [] = []
    splitGroups entries = (takeWhile (/= "") entries):(takeWhile (/= []) $ splitGroups (drop 1 $ dropWhile (/= "") entries))

part1 :: [[String]] -> Int
part1 groups = sum $ map (length . (\(x:xs) -> foldr union x xs) ) groups

part2 :: [[String]] -> Int
part2 groups = sum $ map (length . (\(x:xs) -> foldr intersect x xs) ) groups
