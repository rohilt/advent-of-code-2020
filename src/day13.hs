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
part2 (_, busIds) = sieve $ formatBusIds busIds
  where
    formatBusIds = reverse . fst . foldl formatBusId ([], 0)
    formatBusId (x,i) y = if isNothing y then (x,i+1) else ((fromJust y, i):x, i+1)

sieve :: [(Int, Int)] -> Int
sieve ((i,s):(n,t):xs) = sieve ((i*n,ans):xs)
  where
    ans = head $ dropWhile (not . correctMod) $ [s,s+i..]
    correctMod y = mod y n == mod (n-t) n
sieve [(i,s)] = s
sieve _ = error "This shouldn't happen"
