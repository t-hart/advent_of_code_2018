module Day04 where

type GuardId = Int

type Time = Int -- replace this with UTCTime from Data.Time

data EventType
  = BeginShift
  | FallAsleep
  | WakeUp
  deriving (Show, Eq)

data Event = Event
  { guard :: GuardId
  , eventType :: EventType
  , time :: Time
  } deriving (Show, Eq)

parseInput :: String -> [Event]
parseInput s =
  [Event {guard = length s, time = length s, eventType = BeginShift}]

partOne :: String -> GuardId
partOne = length
