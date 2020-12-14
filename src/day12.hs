import System.IO
import Data.List

data Instruction = North Int | South Int | East Int | West Int | TurnLeft | TurnRight | TurnAround | Forward Int
data Direction = NorthDir | SouthDir | EastDir | WestDir
type ShipPosition = ((Int, Int), Direction)
type ShipPosition' = ((Int, Int), (Int, Int))

main :: IO()
main = do
  input <- readFile "../input/day12.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Instruction]
parseInput = map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction (x:y)
  | x == 'N' = North $ read y
  | x == 'S' = South $ read y
  | x == 'E' = East $ read y
  | x == 'W' = West $ read y
  | x == 'L' && y == "90" = TurnLeft
  | x == 'L' && y == "180" = TurnAround
  | x == 'L' && y == "270" = TurnRight
  | x == 'R' && y == "90" = TurnRight
  | x == 'R' && y == "180" = TurnAround
  | x == 'R' && y == "270" = TurnLeft
  | x == 'F' = Forward $ read y
  | otherwise = error "This shouldn't happen"

part1 :: [Instruction] -> Int
part1 = manhattanDistance . fst . foldl interpretInstruction ((0, 0), EastDir)

part2 :: [Instruction] -> Int
part2 = manhattanDistance . fst . foldl interpretInstruction' ((0, 0), (10, 1))

interpretInstruction :: ShipPosition -> Instruction -> ShipPosition
interpretInstruction ((x, y), d) (North i) = ((x, y+i), d)
interpretInstruction ((x, y), d) (South i) = ((x, y-i), d)
interpretInstruction ((x, y), d) (East i) = ((x+i, y), d)
interpretInstruction ((x, y), d) (West i) = ((x-i, y), d)
interpretInstruction (p, NorthDir) (TurnAround) = (p, SouthDir)
interpretInstruction (p, SouthDir) (TurnAround) = (p, NorthDir)
interpretInstruction (p, EastDir) (TurnAround) = (p, WestDir)
interpretInstruction (p, WestDir) (TurnAround) = (p, EastDir)
interpretInstruction (p, NorthDir) (TurnLeft) = (p, WestDir)
interpretInstruction (p, SouthDir) (TurnLeft) = (p, EastDir)
interpretInstruction (p, EastDir) (TurnLeft) = (p, NorthDir)
interpretInstruction (p, WestDir) (TurnLeft) = (p, SouthDir)
interpretInstruction (p, NorthDir) (TurnRight) = (p, EastDir)
interpretInstruction (p, SouthDir) (TurnRight) = (p, WestDir)
interpretInstruction (p, EastDir) (TurnRight) = (p, SouthDir)
interpretInstruction (p, WestDir) (TurnRight) = (p, NorthDir)
interpretInstruction ((x, y), NorthDir) (Forward i) = ((x, y+i), NorthDir)
interpretInstruction ((x, y), SouthDir) (Forward i) = ((x, y-i), SouthDir)
interpretInstruction ((x, y), EastDir) (Forward i) = ((x+i, y), EastDir)
interpretInstruction ((x, y), WestDir) (Forward i) = ((x-i, y), WestDir)

interpretInstruction' :: ShipPosition' -> Instruction -> ShipPosition'
interpretInstruction' (p, (x, y)) (North i) = (p, (x, y+i))
interpretInstruction' (p, (x, y)) (South i) = (p, (x, y-i))
interpretInstruction' (p, (x, y)) (East i) = (p, (x+i, y))
interpretInstruction' (p, (x, y)) (West i) = (p, (x-i, y))
interpretInstruction' (p, (x, y)) (TurnAround) = (p, (-x, -y))
interpretInstruction' (p, (x, y)) (TurnLeft) = (p, (-y, x))
interpretInstruction' (p, (x, y)) (TurnRight) = (p, (y, -x))
interpretInstruction' ((x, y), (x', y')) (Forward i) = ((x + (x'*i), y + (y'*i)), (x', y'))

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y
