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
    basicRows = map (\f -> f t) [topRow, bottomRow, leftColumn, rightColumn]

getMatchesById :: Input -> [(Int, Int)]
getMatchesById input = [ (k, length $ intersect allPossibleEdges $ getEdges (input Map.! k)) | k <- (Map.keys input) ]
  where
    allPossibleEdges = concat $ map (getEdges . snd) $ Map.toList input

adjustTileInitialCorner :: Input -> Int -> [Pixel] -> Input
adjustTileInitialCorner map i match = Map.insert i newTile map
  where
    tile = (map Map.! i)
    newTile
      | leftColumn tile == match = reflectVertical tile
      | rightColumn tile == match = tile
      | topRow tile == match = reflectHorizontal tile
      | bottomRow tile == match = tile
      | (reverse $ leftColumn tile) == match = rotate180 tile
      | (reverse $ rightColumn tile) == match = reflectHorizontal tile
      | (reverse $ topRow tile) == match = reflectUpDiagonal tile
      | (reverse $ bottomRow tile) == match = rotate270 tile

adjustTileMatchLeft :: Input -> Int -> [Pixel] -> Input -- TODO
adjustTileMatchLeft map i match = Map.insert i newTile map
  where
    tile = (map Map.! i)
    newTile
      | leftColumn tile == match = reflectVertical tile
      | rightColumn tile == match = tile
      | topRow tile == match = reflectHorizontal tile
      | bottomRow tile == match = tile
      | (reverse $ leftColumn tile) == match = rotate180 tile
      | (reverse $ rightColumn tile) == match = reflectHorizontal tile
      | (reverse $ topRow tile) == match = reflectUpDiagonal tile
      | (reverse $ bottomRow tile) == match = rotate270 tile

adjustTileMatchUp :: Input -> Int -> [Pixel] -> Input -- TODO
adjustTileMatchUp map i match = Map.insert i newTile map
  where
    tile = (map Map.! i)
    newTile
      | leftColumn tile == match = reflectVertical tile
      | rightColumn tile == match = tile
      | topRow tile == match = reflectHorizontal tile
      | bottomRow tile == match = tile
      | (reverse $ leftColumn tile) == match = rotate180 tile
      | (reverse $ rightColumn tile) == match = reflectHorizontal tile
      | (reverse $ topRow tile) == match = reflectUpDiagonal tile
      | (reverse $ bottomRow tile) == match = rotate270 tile

reflectHorizontal :: ImageTile -> ImageTile
reflectHorizontal t = [ (t !! i) | i <- (reverse [0..(length t - 1)]) ]

reflectVertical :: ImageTile -> ImageTile
reflectVertical = map (\t' -> [ (t' !! i) | i <- (reverse [0..(length t' - 1)]) ])

reflectUpDiagonal :: ImageTile -> ImageTile
reflectUpDiagonal = reflectDownDiagonal . rotate180

reflectDownDiagonal :: ImageTile -> ImageTile
reflectDownDiagonal = transpose

rotate0 :: ImageTile -> ImageTile
rotate0 = id

rotate90 :: ImageTile -> ImageTile
rotate90 = reflectVertical . reflectDownDiagonal

rotate180 :: ImageTile -> ImageTile
rotate180 = reverse . map reverse

rotate270 :: ImageTile -> ImageTile
rotate270 = reflectHorizontal . reflectDownDiagonal

topRow :: ImageTile -> [Pixel]
topRow t = (t !! 0)

bottomRow :: ImageTile -> [Pixel]
bottomRow t = (t !! (length t - 1))

leftColumn :: ImageTile -> [Pixel]
leftColumn t = map (\t' -> (t' !! 0)) t

rightColumn :: ImageTile -> [Pixel]
rightColumn t = map (\t' -> (t' !! (length t' - 1))) t
