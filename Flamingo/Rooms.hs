module Flamingo.Rooms (Room(Room), exits, description,
                       Direction(North, East, South, West),
                       startingRoom, crampedCloset) where
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.List
import System.IO
import Flamingo.Utils

data Direction = North | East | South | West deriving (Eq, Show, Enum)
data Room = Room { exits :: [(Direction, Room)], description :: String }

startingRoom :: Room
startingRoom = Room { exits = [(North, crampedCloset)], description = "You find yourself in a round room with a pillar in the middle." }

crampedCloset :: Room
crampedCloset = Room { exits = [(South, startingRoom)], description = "You are in a cramped closet." }

instance Show Room where
  show r = description r ++ "\nExits: (" ++ intercalate ", " (map (show . fst) $ exits r) ++ ")"
