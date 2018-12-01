module Day01 where

import Data.Char
import Data.List
import qualified Data.Set as Set

readSigned :: String -> Int
readSigned ('+':x) = read x
readSigned x = read x

partOne :: String -> Int
partOne = sum . map readSigned . lines

findRepeated :: (Num a, Ord a) => a -> Set.Set a -> [a] -> a
findRepeated sum set (x:xs) =
  if Set.member sum set
    then sum
    else findRepeated (sum + x) (Set.insert sum set) xs

partTwo :: String -> Int
partTwo = findRepeated 0 Set.empty . cycle . map readSigned . lines
