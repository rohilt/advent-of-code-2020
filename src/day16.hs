import System.IO
import Data.List
import Data.Char
import Helper.Parse

type Field = Int -> Bool
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
parseRule = parseRanges . words . dropWhile (not . isDigit)

parseRanges :: [String] -> Field
parseRanges (a:"or":b:[]) = \i -> (parseRange a i) || (parseRange b i)

parseRange :: String -> Field
parseRange s = \i -> (i <= max) && (i >= min)
  where
    min = read $ takeWhile isDigit s
    max = read $ tail $ dropWhile isDigit s

part1 :: Input -> Int
part1 (rules, _, tickets) = sum $ map collectInvalidValues tickets
  where
    collectInvalidValues (x:xs) = (if (any (\f -> f x) rules) then 0 else x) + collectInvalidValues xs
    collectInvalidValues [] = 0

part2 :: Input -> Int
part2 _ = 0
