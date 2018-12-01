module Day01 where

import Data.Char
import Data.List

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseSigned :: String -> Int
parseSigned ('+':x) = read x
parseSigned x = read x

partOne :: String -> Int
partOne = sum . map parseSigned . lines

partTwo :: String -> Int
partTwo = partOne
