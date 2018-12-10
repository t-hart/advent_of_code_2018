module Day04Spec where

import qualified Data.Time as Time
import Day04
import Test.Hspec

spec :: Spec
spec = do
  regexSpec
  eventCreationSpec
  sortingSpec
  partOneSpec
  findLongestSleeperSpec
  -- partTwoSpec

regexSpec :: Spec
regexSpec =
  describe "Regex" $ do
    it "mathches correctly" $
      map parseLogEntry regexTestEntries `shouldBe`
      [ ["1518-11-04 00:36", "falls asleep"]
      , ["1518-11-04 00:36", "wakes up"]
      , ["1518-11-04 00:36", "10"]
      ]

eventCreationSpec :: Spec
eventCreationSpec =
  let event = flip Event regexTestDate
   in it "creates events correctly" $ do
        map logEntryToEvent regexTestEntries `shouldBe`
          map Just [event FallAsleep, event WakeUp, event $ BeginShift 10]

findLongestSleeperSpec :: Spec
findLongestSleeperSpec =
  describe "Find longest sleeper" $
  it "finds the longest sleeping guard id and the corresponding minutes" $
  findLongestSleeping examplesSorted `shouldBe` (10, 50)

regexTestEntries :: [String]
regexTestEntries =
  [ "[1518-11-04 00:36] falls asleep"
  , "[1518-11-04 00:36] wakes up"
  , "[1518-11-04 00:36] Guard #10 begins shift"
  ]

regexTestDate :: Time.UTCTime
regexTestDate = Time.UTCTime (Time.fromGregorian 1518 11 04) (36 * 60)

sortingSpec :: Spec
sortingSpec =
  it "sorts the event list by time" $
  parseInput examplesShuffled `shouldBe` parseInput examplesSorted

partOneSpec :: Spec
partOneSpec =
  describe "Part one" $
  it "solves the examples" $ partOne examplesShuffled `shouldBe` 240

-- partTwoSpec :: Spec
-- partTwoSpec =
--   describe "Part two" $ do
--     it "solves the examples" $ partTwo examples `shouldBe` Just 3
examplesShuffled :: String
examplesShuffled =
  "[1518-11-04 00:36] falls asleep\n[1518-11-03 00:24] falls asleep\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-03 00:29] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-02 00:50] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-01 00:55] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:55] wakes up\n[1518-11-02 00:40] falls asleep\n"

examplesSorted :: String
examplesSorted =
  "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up\n"
