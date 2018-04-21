{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd where

import Data.Char
import Data.List

data Sized = Sized String
  deriving (Show)

class Aggregate s where
  data Error s :: *
  data Command s :: *
  data Event s :: *

  execute :: s -> Command s -> Either (Error s) (Event s)
  apply :: s -> Event s -> s
  seed :: s

instance Aggregate Sized where
  data Error Sized = Empty | AlreadyUp | AlreadyDown
    deriving (Show)
  data Command Sized = Grow | Shrink
    deriving (Show)
  data Event Sized = Grew String | Shrunk String
    deriving (Show)

  execute (Sized s) Grow =
    Grew
    <$> validate ((>0) . length) Empty s
    <* validate (not . all isUpper) AlreadyUp s
  execute (Sized s) Shrink =
    Shrunk
    <$> validate ((>0) . length) Empty s
    <* validate (not . all isLower) AlreadyDown s

  apply _s (Grew s) = Sized $ toUpper <$> s
  apply _s (Shrunk s) = Sized $ toLower <$> s
  seed = Sized ""

validate :: (a -> Bool) -> e -> a -> Either e a
validate pred e a
  | pred a = Right a
  | otherwise = Left e
