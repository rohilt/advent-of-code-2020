import System.IO
import Data.List

type Ingredient = String
type Allergen = String
type Food = ([Ingredient], [Allergen])
type AllergenIngredientMap = [(Allergen, [Ingredient])]

main :: IO()
main = do
  input <- readFile "../input/day21.in"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input

parseInput :: String -> [Food]
parseInput = map (parseFood . words) . lines
  where
    findAllergens y = head y /= '('
    parseFood x = (takeWhile findAllergens x, parseAllergens $ dropWhile findAllergens x)
    parseAllergens = map init . tail

part1 :: [Food] -> Int
part1 foods = foldl countSafeIngredients 0 $ map fst foods
  where
    is = getPotentialAllergenIngredients foods
    countSafeIngredients x y = x + (length $ filter (\x -> not $ elem x is) y)

part2 :: [Food] -> String
part2 = intercalate "," . map snd . sortOn fst . (\(_, ais, _) -> ais) . getLastSimplifiedMap

getPotentialAllergenIngredients :: [Food] -> [Ingredient]
getPotentialAllergenIngredients = (\(m, r, _) -> (map snd r) ++ (concat $ map snd m) ) . getLastSimplifiedMap

getLastSimplifiedMap :: [Food] -> (AllergenIngredientMap, [(Allergen, Ingredient)], Bool)
getLastSimplifiedMap = head . dropWhile (\(_, _, d) -> not d) . iterate simplifyMap . (\m -> (m, [], False)) . getAllergenToIngredientsMap

getAllergenToIngredientsMap :: [Food] -> AllergenIngredientMap
getAllergenToIngredientsMap foods = map (\a -> (a, listPotentialIngredients a foods)) $ listAllergens foods

simplifyMap :: (AllergenIngredientMap, [(Allergen, Ingredient)], Bool) -> (AllergenIngredientMap, [(Allergen, Ingredient)], Bool)
simplifyMap (m, is, done)
  | done = (m, is, True)
  | knownAllergen == [] = (m, is, True)
  | otherwise = (newM, (knownAllergen':is), False)
  where
    knownAllergen = map (\(a, i) -> (a, head i)) $ filter ((== 1) . length . snd) m
    knownAllergen' = head knownAllergen
    newM = map (\(a, is') -> (a, filter (/= (snd knownAllergen')) is')) m

listAllergens :: [Food] -> [Allergen]
listAllergens = union' . map snd

listPotentialIngredients :: Allergen -> [Food] -> [Ingredient]
listPotentialIngredients allergen = intersect' . map fst . filter (elem allergen . snd)

union' :: (Eq a) => [[a]] -> [a]
union' x = foldl union [] x

intersect' :: (Eq a) => [[a]] -> [a]
intersect' [] = error "This shouldn't happen"
intersect' x = foldl intersect (head x) (tail x)
