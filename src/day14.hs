import System.IO
import Data.List
import qualified Data.Map as Map

type Mask = [(Int, Bool)]
data ProgramInstruction = SetMask Mask | SetMemory (Int, Int)
  deriving (Show)
type State = (Map.Map Int Int, Mask)

main :: IO()
main = do
  input <- readFile "../input/day14.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [ProgramInstruction]
parseInput = map (formatInstr . words) . lines
  where
    formatInstr ("mask":_:mask:[]) = SetMask $ formatMask (reverse mask) 0
    formatInstr (('m':'e':'m':'[':addr):_:num:[]) = SetMemory (read $ init addr, read num)
    formatMask ('X':xs) i = formatMask xs (i+1)
    formatMask ('1':xs) i = (i,True):(formatMask xs (i+1))
    formatMask ('0':xs) i = (i,False):(formatMask xs (i+1))
    formatMask [] _ = []

part1 :: [ProgramInstruction] -> Int
part1 = sum . map snd . Map.toList . fst . finalState
  where
    finalState = foldl interpretInstruction (Map.empty, [])

part2 :: [ProgramInstruction] -> String
part2 _ = "Not yet implemented"

interpretInstruction :: State -> ProgramInstruction -> State
interpretInstruction (memory, mask) (SetMask newMask) = (memory, newMask)
interpretInstruction (memory, mask) (SetMemory (addr, val)) = (Map.insert addr (newValue val mask) memory, mask)
  where
    valueOfBit num i = mod num (2 ^ (i+1)) >= (2 ^ i)
    adjustBit n (i,b)
      | valueOfBit n i == b = n
      | valueOfBit n i = n - (2 ^ i)
      | not $ valueOfBit n i = n + (2 ^ i)
    newValue val = foldl adjustBit val
