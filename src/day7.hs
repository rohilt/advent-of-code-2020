import System.IO
import Data.List
import Data.Maybe
import qualified Data.Set as Set

main :: IO()
main = do
  input <- readFile "../input/day7.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [(String, String)]
parseInput = concat . map fromJust . filter isJust . map parseLine . lines

parseLine :: String -> Maybe [(String, String)]
parseLine line
  | (length $ wline) == 7 = Nothing
  | otherwise = Just [(container, content) | content <- contents]
  where
    wline = words line
    color = intercalate " " . take 2
    container = color wline
    contents = [color $ drop i $ wline | i <- [5,9..(length wline - 1)] ]

part1 :: [(String, String)] -> Int
part1 edges = Set.size $ dfs edges "shiny gold"

part2 :: [(String, String)] -> String
part2 _ = "Not yet implemented"

dfs :: [(String, String)] -> String -> Set.Set String
dfs edges s = Set.union (Set.fromList directContainers) indirectContainers
  where
    directContainers = map fst $ filter (\(_, c) -> c == s) edges
    indirectContainers = foldl Set.union Set.empty $ map (dfs edges) directContainers
