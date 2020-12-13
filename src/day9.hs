import System.IO
import Data.List
import Data.Maybe

main :: IO()
main = do
  input <- readFile "../input/day9.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: [Int] -> Int
part1 = doesNotMatchProperty

part2 :: [Int] -> Int
part2 xs = matchSum xs $ doesNotMatchProperty xs

doesNotMatchProperty :: [Int] -> Int
doesNotMatchProperty xs = fromJust $ head $ filter isJust [validSum i | i <- [25..(length xs - 1)] ]
  where
    prevSum i = [x + y | x <- prev25 i, y <- prev25 i]
    prev25 i = take 25 $ drop (i-25) xs
    validSum i = if (elem (xs !! i) (prevSum i)) then Nothing else Just (xs !! i)

matchSum :: [Int] -> Int -> Int
matchSum (x:xs) s
  | isJust $ matchSumStart xs (s-x) x x = fromJust $ matchSumStart xs (s-x) x x
  | otherwise = matchSum xs s
matchSum [] _ = error "This shouldn't happen"

matchSumStart :: [Int] -> Int -> Int -> Int -> Maybe Int
matchSumStart (x:xs) s y1 y2
  | s < 0 = Nothing
  | x == s = Just (newMin + newMax)
  | x > s = Nothing
  | x < s = matchSumStart xs (s-x) newMin newMax
  where
    newMin = if y1 > x then x else y1
    newMax = if y2 < x then x else y2
matchSumStart [] _ _ _ = Nothing
