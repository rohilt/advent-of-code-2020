import System.IO
import Data.List
import qualified Data.Set as Set

main :: IO()
main = do
  input <- readFile "input/day6.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [[String]]
parseInput = splitGroups . lines 
  where
    splitGroups [] = []
    splitGroups entries = (takeWhile (/= "") entries):(takeWhile (/= []) $ splitGroups (drop 1 $ dropWhile (/= "") entries))

part1 :: [[String]] -> Int
part1 = sum . map (length . (\(x:xs) -> foldr union x xs) )

part2 :: [[String]] -> Int
part2 = sum . map (length . (\(x:xs) -> foldr intersect x xs) )
