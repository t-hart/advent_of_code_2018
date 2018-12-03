module Day03Spec where

import Day03
import Test.Hspec

spec :: Spec
spec = do
  parserSpec
  partOneSpec
  -- partTwoSpec

parserSpec :: Spec
parserSpec =
  describe "Parser" $
  it "Correctly parses the examples" $
  parseInput examples `shouldBe`
  [ Claim {claimId = 1, position = (1, 3), dimensions = (4, 4)}
  , Claim {claimId = 2, position = (3, 1), dimensions = (4, 4)}
  , Claim {claimId = 3, position = (5, 5), dimensions = (2, 2)}
  ]

partOneSpec :: Spec
partOneSpec =
  describe "Part one" $ it "solves the examples" $ partOne examples `shouldBe` 4

-- partTwoSpec :: Spec
-- partTwoSpec =
--   describe "Part two" $ do
--     it "solves the examples" $
--       partTwo  `shouldBe`
examples :: String
examples = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
