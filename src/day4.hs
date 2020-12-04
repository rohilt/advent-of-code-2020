import System.IO
import Data.List
import qualified Data.Map as Map

type Passport = Map.Map String String

main :: IO()
main = do
  input <- readFile "input/day4.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Passport]
parseInput input = map formatEntries $ splitEntries $ lines input
  where
    splitEntries [] = []
    splitEntries entries = (takeWhile (/= "") entries):(takeWhile (/= []) $ splitEntries (drop 1 $ dropWhile (/= "") entries))
    formatEntries = Map.fromList . map ((\(i,j) -> (i, drop 1 j)) . break (==':')) . words . intercalate " "

part1 :: [Passport] -> Int
part1 passports = length $ filter validPassport passports

part2 :: [Passport] -> String
part2 _ = "Not yet implemented"

validPassport :: Passport -> Bool
validPassport p = and $ map passportContains ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  where
    passportContains k = Map.member k p
