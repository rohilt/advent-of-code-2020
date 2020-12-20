import System.IO
import Data.List

data State = Active | Inactive
  deriving (Show, Eq)
type Plane = [[State]]
type Dimension = [Plane]
type Dimension' = [Dimension]

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
part2 p = countActive' ((iterate (dimensionCycle' . expandDimension') [[p]]) !! 6)

expandDimension :: Dimension -> Dimension
expandDimension x = tempDimensionExpansion ++ (map expandPlane x) ++ tempDimensionExpansion
  where
    expandList :: [State] -> [State]
    expandList x = [Inactive] ++ x ++ [Inactive]
    expandPlane :: [[State]] -> [[State]]
    expandPlane x = [(replicate (length x + 2) Inactive)] ++ (map expandList x) ++ [(replicate (length x + 2) Inactive)]
    tempDimensionExpansion = [replicate (length (head x) + 2) (replicate (length (head (head x)) + 2) Inactive)]

expandDimension' :: Dimension' -> Dimension'
expandDimension' x = tempDimensionExpansion' ++ (map expandDimension x) ++ tempDimensionExpansion'
  where
    tempDimensionExpansion' = [replicate (length (head x) + 2) (replicate (length (head (head x)) + 2) (replicate (length (head (head (head (x)))) + 2) Inactive))]

dimensionCycle :: Dimension -> Dimension
dimensionCycle d = [ [ [ newValue (i,j,k) | i <- [0..columnI] ] | j <- [0..rowI] ] | k <- [0..planeI] ]
  where
    planeI = length d - 1
    rowI = length (d !! 0) - 1
    columnI = length (d !! 0 !! 0) - 1
    numNeighborsActive (i,j,k) = length $ filter (==Active) neighbors
      where
        iRange i = intersect [i-1..i+1] [0..columnI]
        jRange j = intersect [j-1..j+1] [0..rowI]
        kRange k = intersect [k-1..k+1] [0..planeI]
        neighbors = [ if (i,j,k) /= (i',j',k') then d !! k' !! j' !! i' else Inactive | i' <- iRange i, j' <- jRange j, k' <- kRange k ]
    newValue (i,j,k)
      | (d !! k !! j !! i == Active) = if (numNeighborsActive (i,j,k) == 2 || numNeighborsActive (i,j,k) == 3) then Active else Inactive
      | otherwise = if (numNeighborsActive (i,j,k) == 3) then Active else Inactive

dimensionCycle' :: Dimension' -> Dimension'
dimensionCycle' d = [ [ [ [ newValue (i,j,k,l) | i <- [0..columnI] ] | j <- [0..rowI] ] | k <- [0..planeI] ] | l <- [0..dimI] ]
  where
    dimI = length d - 1
    planeI = length (d !! 0) - 1
    rowI = length (d !! 0 !! 0) - 1
    columnI = length (d !! 0 !! 0 !! 0) - 1
    numNeighborsActive (i,j,k,l) = length $ filter (==Active) neighbors
      where
        iRange i = intersect [i-1..i+1] [0..columnI]
        jRange j = intersect [j-1..j+1] [0..rowI]
        kRange k = intersect [k-1..k+1] [0..planeI]
        lRange l = intersect [l-1..l+1] [0..dimI]
        neighbors = [ if (i,j,k,l) /= (i',j',k',l') then d !! l' !! k' !! j' !! i' else Inactive | i' <- iRange i, j' <- jRange j, k' <- kRange k, l' <- lRange l ]
    newValue (i,j,k,l)
      | (d !! l !! k !! j !! i == Active) = if (numNeighborsActive (i,j,k,l) == 2 || numNeighborsActive (i,j,k,l) == 3) then Active else Inactive
      | otherwise = if (numNeighborsActive (i,j,k,l) == 3) then Active else Inactive

countActive' :: Dimension' -> Int
countActive' (x:xs) = countActive x + countActive' xs
countActive' [] = 0

countActive :: Dimension -> Int
countActive (x:xs) = countActivePlane x + countActive xs
countActive [] = 0

countActivePlane :: Plane -> Int
countActivePlane (x:xs) = countActiveList x + countActivePlane xs
  where
    countActiveList = length . filter (== Active)
countActivePlane [] = 0
