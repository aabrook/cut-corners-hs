{-# LANGUAGE TypeFamilies #-}
module Aggregate where

import Data.Function (on)
import Control.Applicative

data EventData e = EventData {
    eventId :: Int,
    body :: Event e
}

instance Show (EventData e) where
    show = show . eventId

instance Eq (EventData e) where
    (==) = (==) `on` eventId

instance Ord (EventData e) where
    compare = compare `on` eventId

class Aggregate s where
    data Error s :: *
    data Command s :: *
    data Event s :: *

    execute :: s -> Command s -> Either (Error s) (Event s)
    apply :: s -> Event s -> s
    seed :: s

