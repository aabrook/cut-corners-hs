module Main where

import Lib
import Prelude
import Data.Monoid ((<>))
import Data.List.Split (splitOn)

data AggregatorId = Id String
  deriving (Show)
data Payload = Text String
  deriving (Show)
data Event = Event String | WithPayload Event Payload
  deriving (Show)
data EventRecord = EventRecord AggregatorId Event
  deriving (Show)

logPath :: String
logPath = "../event-log.txt"

writeToLog :: String -> IO ()
writeToLog = (appendFile logPath) . ((flip (<>)) "\n")

recordEvent :: AggregatorId -> Event -> IO ()
recordEvent (Id id) (Event ev) = writeToLog $ id <> "," <> ev
recordEvent (Id id) (WithPayload (Event ev) (Text payload)) = writeToLog $ id <> "," <> ev <> "," <> payload

toEvent :: [String] -> Maybe EventRecord
toEvent [id, event] = Just $ EventRecord (Id id) (Event event)
toEvent [id, event, payload] = Just $ EventRecord (Id id) (WithPayload (Event event) (Text payload))
toEvent (id:event:xs) = toEvent $ id:event:[(concat xs)]
toEvent _ = Nothing

showLog :: IO [[String]]
showLog =
  map (splitOn ",") . lines <$> readFile logPath

main :: IO ()
main = someFunc
