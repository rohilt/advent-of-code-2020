module Helper.Parse (splitOn, splitNumsAtComma)
where

splitOn :: String -> [String] -> [[String]]
splitOn _ [] = []
splitOn s entries = (takeWhile (/= s) entries):(takeWhile (/= []) $ splitOn s (drop 1 $ dropWhile (/= s) entries))

splitNumsAtComma :: String -> [Int]
splitNumsAtComma input
  | (not $ elem ',' input) = [read input]
  | otherwise = (read $ takeWhile (/= ',') input):(splitNumsAtComma $ tail $ dropWhile (/= ',') input)
