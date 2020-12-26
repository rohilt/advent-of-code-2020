import System.IO
import Data.List
import Helper.Parse

type CardDeck = [Int]
type Input = (CardDeck, CardDeck)

main :: IO()
main = do
  input <- readFile "../input/day22.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> Input
parseInput = listToTuple . map (map read . tail) . splitOn "" . lines
  where
    listToTuple (a:b:[]) = (a,b)

part1 :: Input -> Int
part1 = getScore . winningDeck

part2 :: Input -> String
part2 _ = "Not yet implemented"

playRound :: Input -> Input
playRound ((x:xs), (y:ys))
  | x > y = (xs ++ [x,y], ys)
  | x < y = (xs, ys ++ [y,x])
  | otherwise = error "This shouldn't happen"

winningDeck :: Input -> CardDeck
winningDeck = takeWinningDeck . head . dropWhile (\(x,y) -> x /= [] && y /= []) . iterate playRound
  where
    takeWinningDeck ([], x) = x
    takeWinningDeck (x, []) = x

getScore :: CardDeck -> Int
getScore cards = sum $ [ (length cards - i)*(cards !! i) | i <- [0..(length cards - 1)] ]
