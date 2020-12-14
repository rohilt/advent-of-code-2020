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

part2 :: Information -> String
part2 _ = "Not yet implemented"
