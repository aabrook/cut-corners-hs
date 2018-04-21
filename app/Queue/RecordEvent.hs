module Queue.RecordEvent where

import Prelude (IO, ($))
import Data.ByteString.Lazy (appendFile)
import Data.Aeson
import Aggregate

recordEvent :: ToJSON a => a -> IO()
recordEvent e = appendFile "./log.txt" $ encode e
