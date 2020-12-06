import System.IO
import Data.List

main :: IO()
main = do
  input <- readFile "../input/day2.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [[String]]
parseInput input = map words $ lines input

part1 :: [[String]] -> Int
part1 = length . filter (\b -> b) . map checkRange

part2 :: [[String]] -> Int
part2 = length . filter (\b -> b) . map checkPolicy

checkRange :: [String] -> Bool
checkRange p = (fst r) <= (getCount c s) && (snd r) >= (getCount c s)
  where
    policy = parsePolicy p
    r = (\(r, _, _) -> r) policy
    c = (\(_, c, _) -> c) policy
    s = (\(_, _, s) -> s) policy
    getCount c s = length $ filter (\x -> (x == c)) s

checkPolicy :: [String] -> Bool
checkPolicy p = (start || end) && (not (start && end))
  where
    policy = parsePolicy p
    r = (\(r, _, _) -> r) policy
    c = (\(_, c, _) -> c) policy
    s = (\(_, _, s) -> s) policy
    start = (s !! (fst r - 1)) == c
    end = (s !! (snd r - 1)) == c

parsePolicy :: [String] -> ((Int, Int), Char, String)
parsePolicy [r,c,s] = (getRange r, head c, s)
parsePolicy _ = error "This hopefully shouldn't happen."

getRange :: String -> (Int, Int)
getRange s = (read (fst t) :: Int, read (drop 1 $ snd t) :: Int)
  where t = break (== '-') s
