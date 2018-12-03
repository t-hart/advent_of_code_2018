module Day03 where

import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set

type Dimensions = (Int, Int)

type Position = (Int, Int)

type ClaimId = Int

type ClaimMap = Map.Map Int (Map.Map Int [Int])

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
    parse [cId, at, pos, dim] =
      case (readId cId, sequence [toVector pos, toVector dim]) of
        (Just i, Just [p, d]) ->
          Just $ Claim {claimId = i, position = p, dimensions = d}
        _ -> Nothing
    parse _ = Nothing

parseInput :: String -> [Claim]
parseInput = mapMaybe parseClaim . lines

claimSquare :: ClaimId -> Position -> ClaimMap -> ClaimMap
claimSquare cId (x, y) claims =
  let xs = Map.findWithDefault Map.empty y claims
   in Map.insert y (Map.insertWith (++) x [cId] xs) claims

addClaim :: Claim -> ClaimMap -> ClaimMap
addClaim claim claims =
  let (x, y) = position claim
      cId = claimId claim
      (width, height) = dimensions claim
      squares = [(x, y) | x <- [x .. x + width - 1], y <- [y .. y + height - 1]]
   in foldr (claimSquare cId) claims squares

partOne :: String -> Int
partOne = countOverlaps . createClaimMap . parseInput
  where
    countOverlaps =
      sum . map (length . filter ((<) 1 . length) . Map.elems) . Map.elems

partTwo :: String -> Maybe ClaimId
partTwo input =
  let claims = parseInput input
      claimMap = createClaimMap claims
      overlappingClaims =
        Set.fromList .
        concat . map (concat . filter ((<) 1 . length) . Map.elems) $
        Map.elems claimMap
      ids = Set.fromList $ map claimId claims
   in Set.lookupMin $ Set.difference ids overlappingClaims
