import System.IO
import Data.List

main :: IO()
main = do
  input <- readFile "input/day2.in"
  print $ part1 input
  print $ part2 input

parseInput :: String -> [[String]]
parseInput input = map words $ lines input

part1 :: String -> Int
part1 input = length $ filter (\b -> b) $ map checkRange $ parseInput input

part2 :: String -> Int
part2 input = length $ filter (\b -> b) $ map checkPolicy $ parseInput input

checkRange :: [String] -> Bool
checkRange (r:xs) = and [(fst $ getRange r) <= (getCount xs), (snd $ getRange r) >= (getCount xs)]
checkRange _ = False

checkPolicy :: [String] -> Bool
checkPolicy (r:xs) = (i || j) && (not (i && j))
  where
    i = checkPolicyAt xs (fst $ getRange r)
    j = checkPolicyAt xs (snd $ getRange r)
checkPolicy _ = False

checkPolicyAt :: [String] -> Int -> Bool
checkPolicyAt (c:s) i = (head c) == ((head s) !! (i-1))
checkPolicyAt _ _ = False

getRange :: String -> (Int, Int)
getRange s = (read (fst t) :: Int, read (drop 1 $ snd t) :: Int)
  where t = break (== '-') s

getCount :: [String] -> Int
getCount (c:s) = length $ filter (\x -> (x == (head c))) (head s)
getCount _ = -1
