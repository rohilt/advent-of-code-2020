import System.IO
import Data.List
import Data.Maybe
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
part1 (rules, msgs) = length $ filter (\m -> hasSolution $ matchesRule rules m 0) $ map (\m -> [Just m]) msgs
  where
    hasSolution = (> 0) . length . filter (== (Just ""))

part2 :: Input -> Int
part2 = part1 . adjustMap
  where
    adjust8 = Map.insert 8 $ R [[42],[42,8]]
    adjust11 = Map.insert 11 $ R [[42,31],[42,11,31]]
    adjustMap (map, msgs) = (adjust8 $ adjust11 $ map, msgs)

matchesRule :: RuleMap -> [Maybe String] -> Int -> [Maybe String]
matchesRule _ [] _ = []
matchesRule m (Nothing:ys) i = (Nothing:ys')
  where
    ys' = matchesRule m ys i
matchesRule m ((Just ""):ys) i = (Nothing:ys')
  where
    ys' = matchesRule m ys i
matchesRule m ((Just (x:xs)):ys) i =
  case (m Map.! i) of (C c) -> if (x == c) then (Just xs:ys') else (Nothing:ys')
                      (R subrules) -> (concat $ map (foldl (matchesRule m) [Just (x:xs)]) subrules) ++ ys'
  where
    ys' = matchesRule m ys i
