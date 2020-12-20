import System.IO
import Data.List
import qualified Data.Map as Map
import Helper.Parse

type Pixel = Bool
type ImageTile = [[Pixel]]
type Input = Map.Map Int ImageTile

main :: IO()
main = do
  input <- readFile "../input/day20.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> Input
parseInput = foldl readTile Map.empty . splitOn "" . lines
  where
    readTileId (x:_) = read $ init $ last $ words x
    readPixel '#' = True
    readPixel '.' = False
    readTileContents (_:xs) = map (map readPixel) xs
    readTile i t = Map.insert (readTileId t) (readTileContents t) i

part1 :: Input -> Input
part1 = id

part2 :: Input -> String
part2 _ = "Not yet implemented"
