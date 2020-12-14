import System.IO
import Data.List
import Data.Maybe

type BusId = Maybe Int
type Information = (Int, [BusId])

main :: IO()
main = do
  input <- readFile "../input/day13.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> Information
parseInput = formatInput . words
  where
    formatInput (x:y:[]) = (read x, readIds y)
    readIds "" = []
    readIds (',':y) = readIds y
    readIds y = (readId $ takeWhile (/= ',') y):(readIds $ dropWhile (/= ',') y)
    readId "x" = Nothing
    readId x = Just (read x)

part1 :: Information -> Int
part1 (minTime, busIds) = multiplyTuple $ head $ sortOn snd $ map (waitTime . fromJust) $ filter isJust busIds
  where
    waitTime busId = (busId, busId - (minTime `mod` busId))
    multiplyTuple (x, y) = x*y

part2 :: Information -> Int
part2 (_, busIds) = head $ dropWhile (not . busesMeetCondition) [0..]
  where
    busesMeetCondition t = all (\x -> x) [ if isNothing (busIds !! i) then True else (mod (t+i) (fromJust $ busIds !! i) == 0) | i <- [0..(length busIds - 1)] ]
