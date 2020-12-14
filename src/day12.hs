import System.IO
import Data.List

data Instruction = North Int | South Int | East Int | West Int | TurnLeft | TurnRight | TurnAround | Forward Int
data Direction = NorthDir | SouthDir | EastDir | WestDir
type ShipPosition = ((Int, Int), Direction)

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
  where
    manhattanDistance (x, y) = abs x + abs y

part2 :: [Instruction] -> String
part2 _ = "Not yet implemented"

interpretInstruction :: ShipPosition -> Instruction -> ShipPosition
interpretInstruction ((x, y), d) (North i) = ((x, y+i), d)
interpretInstruction ((x, y), d) (South i) = ((x, y-i), d)
interpretInstruction ((x, y), d) (East i) = ((x+i, y), d)
interpretInstruction ((x, y), d) (West i) = ((x-i, y), d)
interpretInstruction ((x, y), NorthDir) (TurnAround) = ((x, y), SouthDir)
interpretInstruction ((x, y), SouthDir) (TurnAround) = ((x, y), NorthDir)
interpretInstruction ((x, y), EastDir) (TurnAround) = ((x, y), WestDir)
interpretInstruction ((x, y), WestDir) (TurnAround) = ((x, y), EastDir)
interpretInstruction ((x, y), NorthDir) (TurnLeft) = ((x, y), WestDir)
interpretInstruction ((x, y), SouthDir) (TurnLeft) = ((x, y), EastDir)
interpretInstruction ((x, y), EastDir) (TurnLeft) = ((x, y), NorthDir)
interpretInstruction ((x, y), WestDir) (TurnLeft) = ((x, y), SouthDir)
interpretInstruction ((x, y), NorthDir) (TurnRight) = ((x, y), EastDir)
interpretInstruction ((x, y), SouthDir) (TurnRight) = ((x, y), WestDir)
interpretInstruction ((x, y), EastDir) (TurnRight) = ((x, y), SouthDir)
interpretInstruction ((x, y), WestDir) (TurnRight) = ((x, y), NorthDir)
interpretInstruction ((x, y), NorthDir) (Forward i) = ((x, y+i), NorthDir)
interpretInstruction ((x, y), SouthDir) (Forward i) = ((x, y-i), SouthDir)
interpretInstruction ((x, y), EastDir) (Forward i) = ((x+i, y), EastDir)
interpretInstruction ((x, y), WestDir) (Forward i) = ((x-i, y), WestDir)
