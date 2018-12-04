module Day03 where

import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set

type Dimensions = (Int, Int)

type Position = (Int, Int)

type ClaimId = Int

type ClaimMap = Map.Map Position (Set.Set ClaimId)

createClaimMap :: [Claim] -> ClaimMap
createClaimMap = foldr addClaim Map.empty

data Claim = Claim
  { claimId :: ClaimId
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
    parse [cId, _, pos, dim] =
      case (readId cId, sequence [toVector pos, toVector dim]) of
        (Just i, Just [p, d]) ->
          Just $ Claim {claimId = i, position = p, dimensions = d}
        _ -> Nothing
    parse _ = Nothing

parseInput :: String -> [Claim]
parseInput = mapMaybe parseClaim . lines

claimSquare :: ClaimId -> Position -> ClaimMap -> ClaimMap
claimSquare cId pos claims =
  Map.insertWith Set.union pos (Set.singleton cId) claims

addClaim :: Claim -> ClaimMap -> ClaimMap
addClaim claim claims =
  let (left, top) = position claim
      cId = claimId claim
      (width, height) = dimensions claim
      squares =
        [ (x, y)
        | x <- [left .. left + width - 1]
        , y <- [top .. top + height - 1]
        ]
   in foldr (claimSquare cId) claims squares

partOne :: String -> Int
partOne = countOverlaps . createClaimMap . parseInput
  where
    countOverlaps = length . filter ((<) 1 . length) . Map.elems

partTwo :: String -> Maybe ClaimId
partTwo input =
  let claims = parseInput input
      claimMap = createClaimMap claims
      overlappingClaims =
        Set.unions . filter ((<) 1 . length) $ Map.elems claimMap
      ids = Set.fromList $ map claimId claims
   in Set.lookupMin $ Set.difference ids overlappingClaims
