import System.IO
import Data.List
import Data.Maybe
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
part1 = length . filter validPassport

part2 :: [Passport] -> Int
part2 = length . filter (\x -> and [validPassport x, validatePassport x])

validPassport :: Passport -> Bool
validPassport p = all passportContains ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  where
    passportContains k = Map.member k p

validatePassport :: Passport -> Bool
validatePassport p = and [checkByr, checkIyr, checkEyr, checkHgt, checkHcl, checkEcl, checkPid]
  where
    getField s = fromJust $ Map.lookup s p
    byr = read $ getField "byr" :: Int
    checkByr = (byr >= 1920) && (byr <= 2002)
    iyr = read $ getField "iyr" :: Int
    checkIyr = (iyr >= 2010) && (iyr <= 2020)
    eyr = read $ getField "eyr" :: Int
    checkEyr = (eyr >= 2020) && (eyr <= 2030)
    hgt = getField "hgt"
    hgtUnit = drop (length hgt - 2) hgt
    hgtNum = read $ take (length hgt - 2) hgt :: Int
    checkHgt
      | hgtUnit == "cm" = (hgtNum >= 150) && (hgtNum <= 193)
      | hgtUnit == "in" = (hgtNum >= 59) && (hgtNum <= 76)
      | otherwise = False
    hcl = getField "hcl"
    validHcl = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
    checkHcl = (take 1 hcl == "#") && (and $ map (\x -> elem x validHcl) $ drop 1 hcl)
    ecl = getField "ecl"
    validEcl = ["amb","blu","brn","gry","grn","hzl","oth"]
    checkEcl = or $ map (\x -> x == ecl) validEcl
    pid = getField "pid"
    validPid = ['0','1','2','3','4','5','6','7','8','9']
    checkPid = (length pid == 9) && (and $ map (\x -> elem x validPid) pid)
