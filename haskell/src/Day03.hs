module Day03 where

import Data.Maybe

data Claim = Claim
  { id :: Int
  , position :: (Int, Int)
  , dimensions :: (Int, Int)
  } deriving (Show)

parseClaim :: String -> Maybe Claim
parseClaim = parse . words
  where
    parse [claimId, at, pos, dim] =
      Just $ Claim {Day03.id = 1, position = (2, 2), dimensions = (2, 2)}
    parse _ = Nothing

-- parseClaim = const Claim {Day03.id = 1, position = (2, 2), dimensions = (2, 2)}
parseInput :: String -> [Maybe Claim]
parseInput = filter isJust . map parseClaim . lines

partOne :: String -> Int
partOne = length
