{-# LANGUAGE DeriveGeneric #-}

module JsonError where

import Data.Aeson
import GHC.Generics

data JsonError = JsonError { error :: String }
  deriving (Generic, Show)

instance FromJSON JsonError
instance ToJSON JsonError

