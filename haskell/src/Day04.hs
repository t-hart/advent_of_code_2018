module Day04 where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Time as Time
import Text.Regex.PCRE

type GuardId = Int

type Year = Int

type Month = Int

type Day = Int

type Hour = Int

type Minute = Int

logPattern :: String
logPattern = "\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})\\] (.+)"

logPatternBeginsShift :: String
logPatternBeginsShift =
  "\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})\\] Guard #(\\d+) begins shift"

type TimeString = String

toUTCTime :: TimeString -> Maybe Time.UTCTime
toUTCTime timestring =
  case map read $ endByOneOf "- :" timestring of
    [year, month, day, hour, minute] ->
      Just $ makeDate year month day hour minute
    _ -> Nothing

makeDate :: Year -> Month -> Day -> Hour -> Minute -> Time.UTCTime
makeDate year month day hour minute =
  Time.UTCTime
    (Time.fromGregorian (fromIntegral year) month day)
    (Time.secondsToDiffTime $ fromIntegral (hour * (60 ^ 2) + minute * 60))

parseLogEntry :: String -> [String]
parseLogEntry input =
  if '#' `elem` input
    then drop 1 $ head $ input =~ logPatternBeginsShift
    else drop 1 $ head $ input =~ logPattern

logEntryToEvent :: String -> Maybe Event
logEntryToEvent entry =
  case parseLogEntry entry of
    [timestring, "falls asleep"] ->
      toUTCTime timestring >>= (Just . Event FallAsleep)
    [timestring, "wakes up"] -> toUTCTime timestring >>= (Just . Event WakeUp)
    [timestring, guardId] ->
      toUTCTime timestring >>= (Just . Event (BeginShift $ read guardId))
    _ -> Nothing

type Time = Time.UTCTime

data EventType
  = BeginShift GuardId
  | FallAsleep
  | WakeUp
  deriving (Show, Eq)

data Event = Event
  { eventType :: EventType
  , time :: Time
  } deriving (Show, Eq)

parseInput :: String -> [Event]
parseInput =
  sortBy (\a b -> compare (time a) (time b)) . mapMaybe logEntryToEvent . lines

insertSleep ::
     GuardId
  -> Event
  -> Event
  -> Map.Map GuardId Time.NominalDiffTime
  -> Map.Map GuardId Time.NominalDiffTime
insertSleep guardId sleep wake =
  Map.insertWith (+) guardId (Time.diffUTCTime (time wake) (time sleep))

addEvents ::
     GuardId
  -> [Event]
  -> Map.Map GuardId Time.NominalDiffTime
  -> Map.Map GuardId Time.NominalDiffTime
addEvents gId (current:next:rest) m =
  case eventType current of
    BeginShift guardId -> addEvents guardId (next : rest) m
    FallAsleep -> addEvents gId rest $ insertSleep gId current next m
    WakeUp -> addEvents gId (next : rest) m
addEvents _ _ m = m

findLongestSleeping :: String -> (GuardId, Int)
findLongestSleeping =
  (\(guardId, secs) -> (guardId, floor (secs / realToFrac 60) :: Int)) .
  Map.foldlWithKey
    (\acc k v ->
       if v > snd acc
         then (k, v)
         else acc)
    (0, 0) .
  toMap . parseInput

toMap :: [Event] -> Map.Map GuardId Time.NominalDiffTime
toMap events = addEvents 0 events Map.empty

partOne :: String -> Int
-- partOne = foldl (\e m -> Map.insertWith (+) ()) . parseInput
partOne = length . parseInput
