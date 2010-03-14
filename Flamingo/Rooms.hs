module Flamingo.Rooms (Room(Room), exits, description,
                       Direction(North, East, South, West),
                       startingRoom, crampedCloset) where
import Data.Char (toLower)
import Data.List (intercalate)
import Flamingo.Utils

data Direction = North | East | South | West deriving (Eq, Show, Enum)
data Room = Room { exits :: [(Direction, Room)], description :: String }

startingRoom :: Room
startingRoom = Room { exits = [(North, crampedCloset)], description = "You find yourself in a round room with a pillar in the middle." }

crampedCloset :: Room
crampedCloset = Room { exits = [(South, startingRoom)], description = "You are in a cramped closet." }

instance Show Room where
  show r = description r ++ "\nExits: (" ++ intercalate ", " (map (map toLower . show . fst) $ exits r) ++ ")"
