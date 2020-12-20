import System.IO
import Data.List
import Data.Char
import qualified Data.Map as Map
import Helper.Parse

data Rule = C Char | R [[Int]]
type RuleMap = Map.Map Int Rule
type Input = (RuleMap, [String])

main :: IO()
main = do
  input <- readFile "../input/day19.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> Input
parseInput = createInput . splitOn "" . lines
  where
    createInput (rules:msgs:[]) = (foldl parseRule Map.empty rules, msgs)
    createInput _ = error "This shouldn't happen"

parseRule :: RuleMap -> String -> RuleMap
parseRule m s
  | ((=='"') $ head $ last $ words s) = finalMap $ C $ head $ tail $ last $ words s
  | otherwise = finalMap $ R $ map (map read) $ splitOn "|" $ tail $ words s
  where
    ruleNum = read $ takeWhile isDigit s
    finalMap r = Map.insert ruleNum r m

part1 :: Input -> Int
part1 (rules, msgs) = length $ filter (matchesRule rules 0) msgs

part2 :: Input -> String
part2 _ = "Not yet implemented"

matchesRule :: RuleMap -> Int -> String -> Bool
matchesRule _ _ _ = True
