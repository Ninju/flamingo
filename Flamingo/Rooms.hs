module Flamingo.Rooms where
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.List
import Flamingo.Utils

data Environment = Env { connection :: Connection, currentRoom :: Room }
type Direction = String
data Room = Room { exits :: [(Direction, Room)], description :: String }

instance Show Room where
  show r = description r ++ "\nExits: (" ++ intercalate ", " (map fst $ exits r) ++ ")"
