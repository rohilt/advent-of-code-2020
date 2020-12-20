import System.IO
import Data.List
import Data.Maybe
import Data.Char

data Token = Addition | Multiplication | LeftP | RightP | Number Int
  deriving (Show)
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
  | x == '+' = (Addition:(parseInput' xs))
  | x == '*' = (Multiplication:(parseInput' xs))
  | x == '(' = (LeftP:(parseInput' xs))
  | x == ')' = (RightP:(parseInput' xs))
  | otherwise = (Number (read $ takeWhile isDigit (x:xs))):(parseInput' $ dropWhile isDigit (x:xs))
parseInput' [] = []

part1 :: [Expression] -> Int
part1 = sum . map (interpretStackResult . head . foldl interpretExpression [Nothing])
  where
    interpretStackResult (Just (SNumber x)) = x
    interpretStackResult _ = error "This shouldn't happen"

part2 :: [Expression] -> Int
part2 _ = 0

interpretExpression :: Stack -> Token -> Stack
interpretExpression (Nothing:xs) (Number x) = ((Just (SNumber x)):xs)
interpretExpression ((Just (SNumber x)):xs) (Addition) = ((Just (SFunction (+x))):xs)
interpretExpression ((Just (SNumber x)):xs) (Multiplication) = ((Just (SFunction (*x))):xs)
interpretExpression ((Just (SFunction f)):xs) (Number x) = ((Just (SNumber (f x))):xs)
interpretExpression xs LeftP = (Nothing:xs)
interpretExpression ((Just (SNumber x)):Nothing:xs) RightP = ((Just (SNumber x)):xs)
interpretExpression ((Just (SNumber x)):(Just (SFunction f)):xs) RightP = ((Just (SNumber (f x))):xs)
