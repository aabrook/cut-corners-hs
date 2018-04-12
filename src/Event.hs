module Event (
  Command
  , newCmd
  , runIt
  , Event(..)
  , shrinkage
  , test
  ) where

import Data.Char

data Event = Shrunk String
  deriving (Show)

newtype Command a = Command { perform :: a -> Event }

shrinkage = map toLower

newCmd :: (a -> Event) -> Command a
newCmd f = Command { perform = f }

runIt :: Command a -> a -> Event
runIt cmd a = perform cmd a

-- fromEvent :: Events -> Command a b
-- fromEvent (Shrink s) = shrinkage
--

test = runIt (newCmd (Shrunk . shrinkage)) "ABCD"
