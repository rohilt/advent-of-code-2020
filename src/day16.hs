import System.IO
import Data.List
import Data.Char
import Data.Maybe
import Helper.Parse

type Field = (Int -> Bool, Bool)
type Ticket = [Int]
type Input = ([Field], Ticket, [Ticket])

main :: IO()
main = do
  input <- readFile "../input/day16.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> Input
parseInput = parseInfo . splitOn "" . lines

parseInfo :: [[String]] -> Input
parseInfo (r:m:t:[]) = (map parseRule r, splitNumsAtComma $ last m, map splitNumsAtComma $ tail t)
parseInfo _ = error "This shouldn't happen"

parseRule :: String -> Field
parseRule s = (parseRanges $ words $ dropWhile (not . isDigit) s, (== "departure") $ head $ words s)

parseRanges :: [String] -> (Int -> Bool)
parseRanges (a:"or":b:[]) = \i -> (parseRange a i) || (parseRange b i)

parseRange :: String -> (Int -> Bool)
parseRange s = \i -> (i <= max) && (i >= min)
  where
    min = read $ takeWhile isDigit s
    max = read $ tail $ dropWhile isDigit s

part1 :: Input -> Int
part1 (rules, _, tickets) = sum $ map collectInvalidValues tickets
  where
    collectInvalidValues (x:xs) = (if (any (\f -> f x) (map fst rules)) then 0 else x) + collectInvalidValues xs
    collectInvalidValues [] = 0

part2 :: Input -> Int
part2 (rules, myTicket, tickets) = sum $ map ((!!) myTicket) $ findFields rules $ filter (isValidTicket rules) tickets

isValidTicket :: [Field] -> Ticket -> Bool
isValidTicket rules (x:xs) = (any (\f -> f x) (map fst rules)) && (isValidTicket rules xs)
isValidTicket _ [] = True

findFields :: [Field] -> [Ticket] -> [Int]
findFields rules tickets = simplifyFields $ foldl checkValidField start tickets
  where
    start = [ [0..(length rules - 1)] | i <- [0..(length rules - 1)] ]
    checkValidField :: [[Int]] -> Ticket -> [[Int]]
    checkValidField xs t = [ filter (\j -> (fst (rules !! j) (t !! i))) (xs !! i) | i <- [0..(length xs - 1)] ]

simplifyFields :: [[Int]] -> [Int]
simplifyFields fields = map fromJust $ simplifyFields' [ Nothing | i <- [0..(length fields - 1)] ] fields

simplifyFields' :: [Maybe Int] -> [[Int]] -> [Maybe Int]
simplifyFields' s i
  | all ((==0) . length) i = s
  | otherwise = simplifyFields' newS newI
  where
    matchedRule = fromJust $ findIndex ((==1) . length) i
    matchedRuleValue = head $ i !! matchedRule
    newS = (take matchedRule s) ++ [Just matchedRuleValue] ++ (drop (matchedRule+1) s)
    newI = map (delete matchedRuleValue) i
