module Helper.Parse (splitOn)
where

splitOn :: String -> [String] -> [[String]]
splitOn _ [] = []
splitOn s entries = (takeWhile (/= s) entries):(takeWhile (/= []) $ splitOn s (drop 1 $ dropWhile (/= s) entries))
