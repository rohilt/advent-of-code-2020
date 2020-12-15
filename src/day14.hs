import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map

type Mask = [(Int, Maybe Bool)]
type Memory = Map.Map Int Int
data ProgramInstruction = SetMask Mask | SetMemory (Int, Int)
type State = (Memory, Mask)

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
    formatMask ('X':xs) i = (i,Nothing):(formatMask xs (i+1))
    formatMask ('1':xs) i = (i,Just True):(formatMask xs (i+1))
    formatMask ('0':xs) i = (i,Just False):(formatMask xs (i+1))
    formatMask [] _ = []

part1 :: [ProgramInstruction] -> Int
part1 = sum . map snd . Map.toList . fst . foldl interpretInstruction (Map.empty, [])

part2 :: [ProgramInstruction] -> Int
part2 = sum . map snd . Map.toList . fst . foldl interpretInstruction' (Map.empty, [])

interpretInstruction :: State -> ProgramInstruction -> State
interpretInstruction (memory, mask) (SetMask newMask) = (memory, newMask)
interpretInstruction (memory, mask) (SetMemory (addr, val)) = (Map.insert addr (newValue val mask) memory, mask)
  where
    adjustBit n (_,Nothing) = n
    adjustBit n (i,b)
      | valueOfBit n i == (fromJust b) = n
      | valueOfBit n i = n - (2 ^ i)
      | not $ valueOfBit n i = n + (2 ^ i)
    newValue val = foldl adjustBit val


interpretInstruction' :: State -> ProgramInstruction -> State
interpretInstruction' (memory, mask) (SetMask newMask) = (memory, newMask)
interpretInstruction' (memory, mask) (SetMemory (addr, val)) = (foldl addToMemory memory memoryWrites, mask)
  where
    addrToWrite a ((i,Just True):xs)
      | valueOfBit a i = addrToWrite a xs
      | otherwise = addrToWrite (a + (2^i)) xs
    addrToWrite a ((i,Just False):xs) = addrToWrite a xs
    addrToWrite a ((i,Nothing):xs) = (addrToWrite a xs) ++ (addrToWrite (newA a i) xs)
    addrToWrite a [] = [a]
    newA a i
      | valueOfBit a i = a - (2^i)
      | otherwise = a + (2^i)
    memoryWrites = zip (addrToWrite addr mask) (repeat val)
    addToMemory :: Memory -> (Int, Int) -> Memory
    addToMemory m (a,v) = Map.insert a v m

valueOfBit :: Int -> Int -> Bool
valueOfBit num i = mod num (2 ^ (i+1)) >= (2 ^ i)
