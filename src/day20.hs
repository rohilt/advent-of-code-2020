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

part1 :: Input -> Int
part1 = product . take 4 . map fst . sortOn snd . getMatchesById

part2 :: Input -> String
part2 _ = "Not yet implemented"

getEdges :: ImageTile -> [[Pixel]]
getEdges t = basicRows ++ (map reverse basicRows)
  where
    topRow = (t !! 0)
    bottomRow = (t !! (length t - 1))
    leftColumn = map (\t' -> (t' !! 0)) t
    rightColumn = map (\t' -> (t' !! (length t' - 1))) t
    basicRows = [topRow, bottomRow, leftColumn, rightColumn]

getMatchesById :: Input -> [(Int, Int)]
getMatchesById input = [ (k, length $ intersect allPossibleEdges $ getEdges (input Map.! k)) | k <- (Map.keys input) ]
  where
    allPossibleEdges = concat $ map (getEdges . snd) $ Map.toList input
