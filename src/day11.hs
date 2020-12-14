import System.IO
import Data.List
import Data.Array

data GridElement = Floor | Empty | Occupied
  deriving (Eq)
type Grid = Array Int (Array Int GridElement)

main :: IO()
main = do
  input <- readFile "../input/day11.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> Grid
parseInput input = listArray (1, numRows) $ map (listArray (1, numCols) . map interpretChar) $ lines input
  where
    numRows = length $ lines input
    numCols = length $ ((lines input) !! 0)
    interpretChar '.' = Floor
    interpretChar 'L' = Empty
    interpretChar '#' = Occupied

part1 :: Grid -> Int
part1 = countOccupied . getLastGrid transformGrid

part2 :: Grid -> Int
part2 = countOccupied . getLastGrid transformGrid'

transformGrid :: Grid -> Grid
transformGrid grid = grid // [ (i, (grid ! i) // [ (j, newValue (i, j) ) | j <- [1..numCols] ] ) | i <- [1..numRows] ]
  where
    numRows = snd $ bounds grid
    numCols = snd $ bounds (grid ! 1)
    north (i, j) = if (i > 1 && ((grid ! (i-1)) ! j) == Occupied) then 1 else 0
    south (i, j) = if (i < numRows && ((grid ! (i+1)) ! j) == Occupied) then 1 else 0
    east (i, j) = if (j > 1 && ((grid ! i) ! (j-1)) == Occupied) then 1 else 0
    west (i, j) = if (j < numCols && ((grid ! i) ! (j+1)) == Occupied) then 1 else 0
    northeast (i, j) = if (i > 1 && j > 1 && ((grid ! (i-1)) ! (j-1)) == Occupied) then 1 else 0
    northwest (i, j) = if (i > 1 && j < numCols && ((grid ! (i-1)) ! (j+1)) == Occupied) then 1 else 0
    southeast (i, j) = if (i < numRows && j > 1 && ((grid ! (i+1)) ! (j-1)) == Occupied) then 1 else 0
    southwest (i, j) = if (i < numRows && j < numCols && ((grid ! (i+1)) ! (j+1)) == Occupied) then 1 else 0
    directions = [north, south, east, west, northeast, northwest, southeast, southwest]
    occupiedNeighbors (i, j) = (>= 4) $ sum $ map (\f -> f (i, j)) directions
    noOccupiedNeighbors (i, j) = (== 0) $ sum $ map (\f -> f (i, j)) directions
    newValue (i, j)
      | ((grid ! i) ! j) == Floor = Floor
      | ((grid ! i) ! j) == Occupied = if (occupiedNeighbors (i, j)) then Empty else Occupied
      | ((grid ! i) ! j) == Empty = if (noOccupiedNeighbors (i, j)) then Occupied else Empty

transformGrid' :: Grid -> Grid
transformGrid' grid = grid // [ (i, (grid ! i) // [ (j, newValue (i, j) ) | j <- [1..numCols] ] ) | i <- [1..numRows] ]
  where
    numRows = snd $ bounds grid
    numCols = snd $ bounds (grid ! 1)
    north (i, j) = [ ((grid ! i') ! j') | (i', j') <- (zip (reverse [1..i-1]) (repeat j))]
    south (i, j) = [ ((grid ! i') ! j') | (i', j') <- (zip [i+1..numRows] (repeat j))]
    east (i, j) = [ ((grid ! i') ! j') | (i', j') <- (zip (repeat i) [j+1..numCols])]
    west (i, j) = [ ((grid ! i') ! j') | (i', j') <- (zip (repeat i) (reverse [1..j-1]))]
    northeast (i, j) = [ ((grid ! i') ! j') | (i', j') <- (zip (reverse [1..i-1]) [j+1..numCols])]
    northwest (i, j) = [ ((grid ! i') ! j') | (i', j') <- (zip (reverse [1..i-1]) (reverse [1..j-1]))]
    southeast (i, j) = [ ((grid ! i') ! j') | (i', j') <- (zip [i+1..numRows] [j+1..numCols])]
    southwest (i, j) = [ ((grid ! i') ! j') | (i', j') <- (zip [i+1..numRows] (reverse [1..j-1]))]
    directions = [north, south, east, west, northeast, northwest, southeast, southwest]
    interpretSeat Occupied = 1
    interpretSeat Empty = 0
    interpretDirection' = interpretSeat . head . dropWhile (== Floor)
    interpretDirection x = if ((== 0) $ length $ dropWhile (== Floor) x) then 0 else interpretDirection' x
    occupiedNeighbors (i, j) = (>= 5) $ sum $ map (\f -> interpretDirection $ f (i, j)) directions
    noOccupiedNeighbors (i, j) = (== 0) $ sum $ map (\f -> interpretDirection $ f (i, j)) directions
    newValue (i, j)
      | ((grid ! i) ! j) == Floor = Floor
      | ((grid ! i) ! j) == Occupied = if (occupiedNeighbors (i, j)) then Empty else Occupied
      | ((grid ! i) ! j) == Empty = if (noOccupiedNeighbors (i, j)) then Occupied else Empty

getLastGrid :: (Grid -> Grid) -> Grid -> Grid
getLastGrid transform = getLast . iterate transform
  where
    getLast (x:y:xs)
      | (x == y) = x
      | otherwise = getLast (y:xs)
    getLast _ = error "This shouldn't happen"

countOccupied :: Grid -> Int
countOccupied = sum . map (sum . map isOccupied . elems) . elems
  where
    isOccupied Occupied = 1
    isOccupied _ = 0
