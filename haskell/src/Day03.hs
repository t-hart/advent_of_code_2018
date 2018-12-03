module Day03 where

import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe

type Dimensions = (Int, Int)

type Position = (Int, Int)

type ClaimMap = Map.Map Int (Map.Map Int Int)

data Claim = Claim
  { claimId :: Int
  , position :: Position
  , dimensions :: Dimensions
  } deriving (Show, Eq)

readId :: String -> Maybe Int
readId ('#':num) = Just (read num)
readId _ = Nothing

toVector :: String -> Maybe (Int, Int)
toVector = readComponents . endByOneOf ",:x"
  where
    readComponents [x, y] = Just (read x, read y)
    readComponents _ = Nothing

parseClaim :: String -> Maybe Claim
parseClaim = parse . words
  where
    parse [cId, at, pos, dim] =
      case (readId cId, sequence [toVector pos, toVector dim]) of
        (Just i, Just [p, d]) ->
          Just $ Claim {claimId = i, position = p, dimensions = d}
        _ -> Nothing
    parse _ = Nothing

parseInput :: String -> [Claim]
parseInput = mapMaybe parseClaim . lines

addSquare :: Position -> ClaimMap -> ClaimMap
addSquare (x, y) claims =
  let xs = Map.findWithDefault Map.empty y claims
   in Map.insert y (Map.insertWith (+) x 1 xs) claims

addClaim :: Claim -> ClaimMap -> ClaimMap
addClaim claim claims =
  let (x, y) = position claim
      (width, height) = dimensions claim
      squares = [(x, y) | x <- [x .. x + width - 1], y <- [y .. y + height - 1]]
   in foldr addSquare claims squares

partOne :: String -> Int
partOne = countOverlaps . toMap . parseInput
  where
    toMap = foldr addClaim Map.empty
    countOverlaps = sum . map (length . filter ((<) 1) . Map.elems) . Map.elems
