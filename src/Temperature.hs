{-# LANGUAGE OverloadedStrings #-}
module Temperature where

import Data.Aeson
import Control.Applicative
import Control.Monad

data Reading = Reading {
   temperature :: String
  , humidity :: String
  , room :: String
  } deriving (Show)

instance FromJSON Reading where
  parseJSON (Object v) =
    Reading <$> v .: "t"
            <*> v .: "h"
            <*> v .: "r"
  parseJSON _ = mzero

instance ToJSON Reading where
  toJSON (Reading temperature humidity room) =
    object [ "t" .= temperature
      , "h" .= humidity
      , "r" .= room
      ]

