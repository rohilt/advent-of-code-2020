import System.IO

main :: IO()
main = do
  input <- readFile "day1.in"
  print $ part1 input
  print $ part2 input

parseInput :: String -> [Int]
parseInput input = map (\x -> read x :: Int) $ lines input

part1 :: String -> Int
part1 input = findProduct (parseInput input) [] 2

part2 :: String -> Int
part2 input = findProduct (parseInput input) [] 3

findProduct :: [Int] -> [Int] -> Int -> Int
findProduct [] selected s = if selectedWorks then (product selected) else 0
  where
    selectedWorks = and [(length selected) == s, (sum selected) == 2020]
findProduct (x:xs) selected s = if canSelectMore then (if (pickNextOne /= 0) then pickNextOne else skipNextOne) else (if checkSum then finalAnswer else 0)
  where
    canSelectMore = (length selected) < s
    pickNextOne = findProduct xs (x:selected) s
    skipNextOne = findProduct xs selected s
    checkSum = (sum selected) == 2020
    finalAnswer = product selected
