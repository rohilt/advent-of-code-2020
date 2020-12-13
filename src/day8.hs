import System.IO
import Data.List
import Data.Maybe

data Instruction = Acc Int | Jmp Int | Nop Int
type BootState = (Int, [Int])

main :: IO()
main = do
  input <- readFile "../input/day8.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Instruction]
parseInput = map (createInstruction . words) . lines
  where
    createInstruction ("acc":num:[]) = Acc (sign num * (read (extractNum num) :: Int))
    createInstruction ("jmp":num:[]) = Jmp (sign num * (read (extractNum num) :: Int))
    createInstruction ("nop":num:[]) = Nop (sign num * (read (extractNum num) :: Int))
    createInstruction _ = error "This shouldn't happen"
    extractNum = drop 1
    sign ('+':_) = 1
    sign ('-':_) = -1

part1 :: [Instruction] -> Int
part1 = getAccBeforeLoop (0, [0])

part2 :: [Instruction] -> Int
part2 = fromJust . head . filter isJust . map (getAccIfTerminates (0, [0])) . modifiedInstructions

getAccBeforeLoop :: BootState -> [Instruction] -> Int
getAccBeforeLoop state instructions
  | elem (nextInstruction currInstruction) (snd state) = fst state
  | otherwise = getAccBeforeLoop (nextAcc currInstruction, (nextInstruction currInstruction : snd state) ) instructions
  where
    currInstruction = instructions !! (getInstruction state)
    nextInstruction (Jmp i) = getInstruction state + i
    nextInstruction _ = getInstruction state + 1
    nextAcc (Acc i) = fst state + i
    nextAcc _ = fst state

getAccIfTerminates :: BootState -> [Instruction] -> Maybe Int
getAccIfTerminates state instructions
  | elem (nextInstruction currInstruction) (snd state) = Nothing
  | (nextInstruction currInstruction) == (length instructions) = Just (nextAcc currInstruction)
  | otherwise = getAccIfTerminates (nextAcc currInstruction, (nextInstruction currInstruction : snd state) ) instructions
  where
    currInstruction = instructions !! (getInstruction state)
    nextInstruction (Jmp i) = getInstruction state + i
    nextInstruction _ = getInstruction state + 1
    nextAcc (Acc i) = fst state + i
    nextAcc _ = fst state

changeInstruction :: Instruction -> Instruction
changeInstruction (Jmp i) = (Nop i)
changeInstruction (Nop i) = (Jmp i)
changeInstruction i = i

modifiedInstructions :: [Instruction] -> [[Instruction]]
modifiedInstructions instructions = [(take i instructions) ++ [changeInstruction $ instructions !! i] ++ (drop (i+1) instructions) | i <- [0..(length instructions - 1)] ]

getInstruction :: BootState -> Int
getInstruction = head . snd
