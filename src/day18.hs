import System.IO
import Data.List
import Data.Maybe
import Data.Char

data Token = Addition | Multiplication | LeftP | RightP | Number Int
  deriving (Eq)
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

part2 :: [Expression] -> Int
part2 = sum . map (interpretStackResult . head . foldl interpretExpression [Nothing] . addAdditionPrecedence)

interpretExpression :: Stack -> Token -> Stack
interpretExpression (Nothing:xs) (Number x) = ((Just (SNumber x)):xs)
interpretExpression ((Just (SNumber x)):xs) (Addition) = ((Just (SFunction (+x))):xs)
interpretExpression ((Just (SNumber x)):xs) (Multiplication) = ((Just (SFunction (*x))):xs)
interpretExpression ((Just (SFunction f)):xs) (Number x) = ((Just (SNumber (f x))):xs)
interpretExpression xs LeftP = (Nothing:xs)
interpretExpression ((Just (SNumber x)):Nothing:xs) RightP = ((Just (SNumber x)):xs)
interpretExpression ((Just (SNumber x)):(Just (SFunction f)):xs) RightP = ((Just (SNumber (f x))):xs)

addAdditionPrecedence :: Expression -> Expression
addAdditionPrecedence [] = []
addAdditionPrecedence x = (LeftP:(addAdditionPrecedence' x)) ++ [RightP]

addAdditionPrecedence' :: Expression -> Expression
addAdditionPrecedence' [] = []
addAdditionPrecedence' (Addition:xs) = (Addition:(addAdditionPrecedence' xs))
addAdditionPrecedence' ((Number x):xs) = ((Number x):(addAdditionPrecedence' xs))
addAdditionPrecedence' (Multiplication:xs) = (RightP:Multiplication:LeftP:(addAdditionPrecedence' xs))
addAdditionPrecedence' (LeftP:xs) = (LeftP:(addAdditionPrecedence untilRightP)) ++ (addAdditionPrecedence' afterRightP)
  where
    nestedParentheses curr LeftP = curr + 1
    nestedParentheses curr RightP = curr - 1
    nestedParentheses curr _ = curr
    untilMatchingP = length $ takeWhile (>0) $ tail $ scanl nestedParentheses 1 xs
    untilRightP = take untilMatchingP xs
    afterRightP = drop untilMatchingP xs
addAdditionPrecedence' (RightP:xs) = (RightP:(addAdditionPrecedence' xs))

interpretStackResult :: Maybe StackFrame -> Int
interpretStackResult (Just (SNumber x)) = x
interpretStackResult _ = error "This shouldn't happen"
