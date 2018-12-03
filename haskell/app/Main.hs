module Main where

import qualified Day03

putStrLnExtra :: String -> IO ()
putStrLnExtra s = putStrLn $ s ++ "\n"

solve :: (Show a) => (String -> a) -> FilePath -> String -> IO ()
solve f path info = do
  contents <- readFile path
  putStrLnExtra $ info ++ ": " ++ show (f contents)

main :: IO ()
main = do
  putStrLnExtra "-- Solutions --"
  solve Day03.partOne "puzzleInputs/day03.txt" "Day 03 part 1"
  solve Day03.partTwo "puzzleInputs/day03.txt" "Day 03 part 2"
  putStrLnExtra "-- End --"
