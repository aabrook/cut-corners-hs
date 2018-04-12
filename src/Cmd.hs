module Cmd where

import Data.Char

data Event = Shrunk String | Grew String
  deriving (Show)

data Sized = Shrink String | Grow String
  deriving (Show)

class Cmd cmd where
  perform :: cmd -> (cmd, b)

instance Cmd (Sized, Event) where
  perform (Shrink a) = (Shrink a, Shrunk (toLower <$> a))
  perform (Grow s) = (Grow s, Grew (toUpper <$> s))

