import System.IO
import Data.List

data State = Active | Inactive
  deriving (Show, Eq)
type Plane = [[State]]
type Dimension = [Plane]

main :: IO()
main = do
  input <- readFile "../input/day17.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> Plane
parseInput = map (map convertToState) . lines
  where
    convertToState '#' = Active
    convertToState '.' = Inactive

part1 :: Plane -> Int
part1 p = countActive ((iterate (dimensionCycle . expandDimension) [p]) !! 6)

part2 :: Plane -> Int
part2 _ = 0

expandDimension :: Dimension -> Dimension
expandDimension x = tempDimensionExpansion ++ (map expandPlane x) ++ tempDimensionExpansion
  where
    expandList :: [State] -> [State]
    expandList x = [Inactive] ++ x ++ [Inactive]
    expandPlane :: [[State]] -> [[State]]
    expandPlane x = [(replicate (length x + 2) Inactive)] ++ (map expandList x) ++ [(replicate (length x + 2) Inactive)]
    tempDimensionExpansion = [replicate (length (head x) + 2) (replicate (length (head x) + 2) Inactive)]

dimensionCycle :: Dimension -> Dimension
dimensionCycle d = [ [ [ newValue (i,j,k) | i <- [0..columnI] ] | j <- [0..rowI] ] | k <- [0..planeI] ]
  where
    planeI = length d - 1
    rowI = length (d !! 0) - 1
    columnI = length (d !! 0 !! 0) - 1
    below (_,_,k) = k > 0
    above (_,_,k) = k < planeI
    up (_,j,_) = j > 0
    down (_,j,_) = j < rowI
    left (i,_,_) = i > 0
    right (i,_,_) = i < columnI
    n0 (i,j,k) = if below (i,j,k) && (d !! (k-1) !! j !! i) == Active then 1 else 0
    n1 (i,j,k) = if above (i,j,k) && (d !! (k+1) !! j !! i) == Active then 1 else 0
    n2 (i,j,k) = if up (i,j,k) && (d !! k !! (j-1) !! i) == Active then 1 else 0
    n3 (i,j,k) = if down (i,j,k) && (d !! k !! (j+1) !! i) == Active then 1 else 0
    n4 (i,j,k) = if left (i,j,k) && (d !! k !! j !! (i-1)) == Active then 1 else 0
    n5 (i,j,k) = if right (i,j,k) && (d !! k !! j !! (i+1)) == Active then 1 else 0
    n6 (i,j,k) = if below (i,j,k) && up (i,j,k) && (d !! (k-1) !! (j-1) !! i) == Active then 1 else 0
    n7 (i,j,k) = if below (i,j,k) && left (i,j,k) && (d !! (k-1) !! j !! (i-1)) == Active then 1 else 0
    n8 (i,j,k) = if up (i,j,k) && left (i,j,k) && (d !! k !! (j-1) !! (i-1)) == Active then 1 else 0
    n9 (i,j,k) = if below (i,j,k) && down (i,j,k) && (d !! (k-1) !! (j+1) !! i) == Active then 1 else 0
    n10 (i,j,k) = if below (i,j,k) && right (i,j,k) && (d !! (k-1) !! j !! (i+1)) == Active then 1 else 0
    n11 (i,j,k) = if down (i,j,k) && right (i,j,k) && (d !! k !! (j+1) !! (i+1)) == Active then 1 else 0
    n12 (i,j,k) = if above (i,j,k) && up (i,j,k) && (d !! (k+1) !! (j-1) !! i) == Active then 1 else 0
    n13 (i,j,k) = if above (i,j,k) && left (i,j,k) && (d !! (k+1) !! j !! (i-1)) == Active then 1 else 0
    n14 (i,j,k) = if above (i,j,k) && down (i,j,k) && (d !! (k+1) !! (j+1) !! i) == Active then 1 else 0
    n15 (i,j,k) = if above (i,j,k) && right (i,j,k) && (d !! (k+1) !! j !! (i+1)) == Active then 1 else 0
    n16 (i,j,k) = if up (i,j,k) && right (i,j,k) && (d !! k !! (j-1) !! (i+1)) == Active then 1 else 0
    n17 (i,j,k) = if down (i,j,k) && left (i,j,k) && (d !! k !! (j+1) !! (i-1)) == Active then 1 else 0
    n18 (i,j,k) = if below (i,j,k) && up (i,j,k) && left (i,j,k) && (d !! (k-1) !! (j-1) !! (i-1)) == Active then 1 else 0
    n19 (i,j,k) = if below (i,j,k) && up (i,j,k) && right (i,j,k) && (d !! (k-1) !! (j-1) !! (i+1)) == Active then 1 else 0
    n20 (i,j,k) = if below (i,j,k) && down (i,j,k) && left (i,j,k) && (d !! (k-1) !! (j+1) !! (i-1)) == Active then 1 else 0
    n21 (i,j,k) = if below (i,j,k) && down (i,j,k) && right (i,j,k) && (d !! (k-1) !! (j+1) !! (i+1)) == Active then 1 else 0
    n22 (i,j,k) = if above (i,j,k) && up (i,j,k) && left (i,j,k) && (d !! (k+1) !! (j-1) !! (i-1)) == Active then 1 else 0
    n23 (i,j,k) = if above (i,j,k) && up (i,j,k) && right (i,j,k) && (d !! (k+1) !! (j-1) !! (i+1)) == Active then 1 else 0
    n24 (i,j,k) = if above (i,j,k) && down (i,j,k) && left (i,j,k) && (d !! (k+1) !! (j+1) !! (i-1)) == Active then 1 else 0
    n25 (i,j,k) = if above (i,j,k) && down (i,j,k) && right (i,j,k) && (d !! (k+1) !! (j+1) !! (i+1)) == Active then 1 else 0
    numNeighborsActive c = sum $ map (\f -> f c) [n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,n16,n17,n18,n19,n20,n21,n22,n23,n24,n25]
    newValue (i,j,k)
      | (d !! k !! j !! i == Active) = if (numNeighborsActive (i,j,k) == 2 || numNeighborsActive (i,j,k) == 3) then Active else Inactive
      | otherwise = if (numNeighborsActive (i,j,k) == 3) then Active else Inactive

countActive :: Dimension -> Int
countActive (x:xs) = countActivePlane x + countActive xs
countActive [] = 0

countActivePlane :: Plane -> Int
countActivePlane (x:xs) = countActiveList x + countActivePlane xs
  where
    countActiveList = length . filter (== Active)
countActivePlane [] = 0
