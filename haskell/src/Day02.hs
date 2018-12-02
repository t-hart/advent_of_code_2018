module Day02 where

import qualified Data.Map.Strict as Map

-- part one
type Occurrences = (Bool, Bool)

type CharMap = Map.Map Char Int

createCharMap :: String -> CharMap
createCharMap = foldl (\charMap c -> Map.insertWith (+) c 1 charMap) Map.empty

countOccurences :: String -> Occurrences
countOccurences s =
  let elems = Map.elems (createCharMap s)
   in (2 `elem` elems, 3 `elem` elems)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

partOne :: String -> Int
partOne =
  uncurry (*) .
  mapTuple length . mapTuple (filter id) . unzip . map countOccurences . lines

-- part two
sharedChars :: (String, String) -> String
sharedChars = map fst . filter (uncurry (==)) . uncurry zip

findIds :: [(String, String)] -> Maybe String
findIds [] = Nothing
findIds (x:xs) =
  let shared = sharedChars x
   in if length shared == length (fst x) - 1
        then Just shared
        else findIds xs

partTwo :: String -> Maybe String
partTwo = findIds . permutations . lines
  where
    permutations xs = [(x, y) | x <- xs, y <- xs]
