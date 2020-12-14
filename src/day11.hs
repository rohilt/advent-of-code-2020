import System.IO
import Data.List
import Data.Array

data GridElement = Floor | Empty | Occupied
  deriving (Eq)
type Grid = [[GridElement]]

main :: IO()
main = do
  input <- readFile "../input/day11.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> Grid
parseInput input = map (map interpretChar) $ lines input
  where
    interpretChar '.' = Floor
    interpretChar 'L' = Empty
    interpretChar '#' = Occupied

part1 :: Grid -> Int
part1 = countOccupied . getLastGrid

part2 :: Grid -> String
part2 _ = "Not yet implemented"

transformGrid :: Grid -> Grid
transformGrid grid = [ [ newValue (i, j) | j <- [0..(numCols - 1)]] | i <- [0..(numRows - 1)]]
  where
    numRows = length grid
    numCols = length (grid !! 0)
    north (i, j) = if (i > 0 && ((grid !! (i-1)) !! j) == Occupied) then 1 else 0
    south (i, j) = if (i < (numRows-1) && ((grid !! (i+1)) !! j) == Occupied) then 1 else 0
    east (i, j) = if (j > 0 && ((grid !! i) !! (j-1)) == Occupied) then 1 else 0
    west (i, j) = if (j < (numCols-1) && ((grid !! i) !! (j+1)) == Occupied) then 1 else 0
    northeast (i, j) = if (i > 0 && j > 0 && ((grid !! (i-1)) !! (j-1)) == Occupied) then 1 else 0
    northwest (i, j) = if (i > 0 && j < (numCols-1) && ((grid !! (i-1)) !! (j+1)) == Occupied) then 1 else 0
    southeast (i, j) = if (i < (numRows-1) && j > 0 && ((grid !! (i+1)) !! (j-1)) == Occupied) then 1 else 0
    southwest (i, j) = if (i < (numRows-1) && j < (numCols-1) && ((grid !! (i+1)) !! (j+1)) == Occupied) then 1 else 0
    directions = [north, south, east, west, northeast, northwest, southeast, southwest]
    occupiedNeighbors (i, j) = (>= 4) $ sum $ map (\f -> f (i, j)) directions
    noOccupiedNeighbors (i, j) = (== 0) $ sum $ map (\f -> f (i, j)) directions
    newValue (i, j)
      | ((grid !! i) !! j) == Floor = Floor
      | ((grid !! i) !! j) == Occupied = if (occupiedNeighbors (i, j)) then Empty else Occupied
      | ((grid !! i) !! j) == Empty = if (noOccupiedNeighbors (i, j)) then Occupied else Empty

getLastGrid :: Grid -> Grid
getLastGrid = getLast . iterate transformGrid
  where
    getLast (x:y:xs)
      | (x == y) = x
      | otherwise = getLast (y:xs)
    getLast _ = error "This shouldn't happen"

countOccupied :: Grid -> Int
countOccupied = sum . map (sum . map isOccupied)
  where
    isOccupied Occupied = 1
    isOccupied _ = 0
