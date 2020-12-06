import System.IO

main :: IO()
main = do
  input <- readFile "../input/day1.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput input = map (\x -> read x :: Int) $ lines input

part1 :: [Int] -> Int
part1 input = findProduct input [] 2

part2 :: [Int] -> Int
part2 input = findProduct input [] 3

findProduct :: [Int] -> [Int] -> Int -> Int
findProduct [] selected s = if selectedWorks then (product selected) else 0
  where
    selectedWorks = (length selected) == s && (sum selected) == 2020
findProduct (x:xs) selected s = if canSelectMore then (if (pickNextOne /= 0) then pickNextOne else skipNextOne) else (if checkSum then finalAnswer else 0)
  where
    canSelectMore = (length selected) < s
    pickNextOne = findProduct xs (x:selected) s
    skipNextOne = findProduct xs selected s
    checkSum = (sum selected) == 2020
    finalAnswer = product selected
