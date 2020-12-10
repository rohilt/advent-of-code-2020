import System.IO
import Data.List
import Data.Maybe
import qualified Data.Set as Set

type Rule = (String, String, Int)

main :: IO()
main = do
  input <- readFile "../input/day7.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Rule]
parseInput = concat . map fromJust . filter isJust . map parseLine . lines

part1 :: [Rule] -> Int
part1 rules = Set.size $ dfs rules "shiny gold"

part2 :: [Rule] -> Int
part2 rules = (getNumBags rules "shiny gold") - 1

parseLine :: String -> Maybe [Rule]
parseLine line
  | (length $ wline) == 7 = Nothing
  | otherwise = Just [(container, content, num) | (content, num) <- contents]
  where
    wline = words line
    color = intercalate " " . take 2
    container = color wline
    contents = [(color $ drop i wline, read $ head $ drop (i-1) wline :: Int) | i <- [5,9..(length wline - 1)] ]

dfs :: [Rule] -> String -> Set.Set String
dfs rules s = Set.union (Set.fromList directContainers) indirectContainers
  where
    directContainers = map getColor $ filter (\r -> (getColor2 r) == s) rules
    indirectContainers = foldl Set.union Set.empty $ map (dfs rules) directContainers

getNumBags :: [Rule] -> String -> Int
getNumBags rules s = (+1) $ sum $ map recurseRule matchingRules
  where
    matchingRules = filter (\r -> (getColor r) == s) rules
    recurseRule r = (getNum r) * ( (getNumBags rules) $ getColor2 r)

getColor :: Rule -> String
getColor (c, _, _) = c

getColor2 :: Rule -> String
getColor2 (_, c, _) = c

getNum :: Rule -> Int
getNum (_, _, n) = n
