{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Queue.TemperatureAgg where

import Prelude
import Data.Char (isDigit)
import Data.ByteString.Lazy (ByteString, appendFile, unpack)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson
import Aggregate
import Temperature
import Data.Monoid
import JsonError

instance Aggregate Reading where
  data Error Reading = ParseError
                      | TemperatureNotNumber
                      | TemperatureOutOfRange
                      | HumidityNotNumber
                      | HumidityOutOfRange
    deriving (Show)
  data Command Reading = Record ByteString
    deriving (Show)
  data Event Reading = Recorded Reading
    deriving (Show)

  -- execute :: Reading -> Command Reading -> Either (Error) (Event)
  execute _ (Record payload) =
    let
      parse = decode payload :: Maybe Reading
      parseResult = maybe (Left ParseError) (Right) parse
    in
      do
        reading <- parseResult
        validate TemperatureNotNumber (all isDigit) (temperature reading)
        validate TemperatureOutOfRange (inRange (-30) 60) (temperature reading)
        validate HumidityNotNumber (all isDigit) (humidity reading)
        validate HumidityOutOfRange (inRange (-1) 101) (humidity reading)
        return $ Recorded reading

  -- apply :: Reading -> Event Reading -> Reading
  apply _ (Recorded r) = r

  -- seed :: Reading
  seed = Reading "0" "0" "-"

inRange :: Int -> Int -> String -> Bool
inRange min max s = min <= i && i <= max
  where i = read s

validate :: Error e -> (a -> Bool) -> a -> Either (Error e) a
validate e f r = case f r of False -> Left e
                             True -> Right r

egTemp :: ByteString
egTemp = "{ \"h\": \"50\", \"t\": \"10\", \"r\": \"m\" }"

errorMessage :: Error Reading -> String
errorMessage = show . encode . JsonError . show

main = do
  reading <- return $ do
      rec <- execute seed (Record egTemp)
      applied <- return $ apply seed rec
      return . encode $ toJSON applied
  Prelude.appendFile "./log.txt" ("\n" <> (either (errorMessage) (show) reading))
  print reading
