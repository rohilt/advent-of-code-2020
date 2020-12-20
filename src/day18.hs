import System.IO
import Data.List
import Data.Maybe
import Data.Char

data Token = Addition | Multiplication | LeftP | RightP | Number Int
type Expression = [Token]
data StackFrame = SNumber Int | SFunction (Int -> Int)
type Stack = [Maybe StackFrame]

main :: IO()
main = do
  input <- readFile "../input/day18.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Expression]
parseInput = map parseInput' . lines

parseInput' :: String -> Expression
parseInput' (x:xs)
  | x == ' ' = parseInput' xs
  | x == '+' = Addition:(parseInput' xs)
  | x == '*' = Multiplication:(parseInput' xs)
  | x == '(' = LeftP:(parseInput' xs)
  | x == ')' = LeftP:(parseInput' xs)
  | otherwise = (Number (read $ takeWhile isDigit (x:xs))):(parseInput' $ dropWhile isDigit (x:xs))
parseInput' [] = []

part1 :: [Expression] -> Int
part1 = sum . map (interpretStackResult . head . foldl interpretExpression [Nothing])
  where
    interpretStackResult (Just (SNumber x)) = x
    interpretStackResult _ = error "This shouldn't happen"

part2 :: [Expression] -> String
part2 _ = "Not yet implemented"

interpretExpression :: Stack -> Token -> Stack
interpretExpression s _ = s
