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
part2 xs = fromJust $ matchSum xs doesNotMatchProperty' Nothing Nothing
  where
    doesNotMatchProperty' = doesNotMatchProperty xs

doesNotMatchProperty :: [Int] -> Int
doesNotMatchProperty xs = fromJust $ head $ filter isJust [if (elem (xs !! i) (prevSum i)) then Nothing else Just (xs !! i) | i <- [25..(length xs - 1)] ]
  where
    prevSum i = [x + y | x <- take 25 $ drop (i-25) xs, y <- take 25 $ drop (i-25) xs]

matchSum :: [Int] -> Int -> Maybe Int -> Maybe Int -> Maybe Int
matchSum (x:xs) s Nothing Nothing
  | x >= s = Nothing
  | isJust $ matchSum xs (s-x) (Just x) (Just x) = matchSum xs (s-x) (Just x) (Just x)
  | otherwise = matchSum xs s Nothing Nothing
matchSum (x:xs) s (Just y1) (Just y2)
  | x == s = Just (newMin + newMax)
  | x > s = Nothing
  | x < s = matchSum xs (s-x) (Just newMin) (Just newMax)
  where
    newMin = if y1 > x then x else y1
    newMax = if y2 < x then x else y2
matchSum [] _ _ _ = Nothing
