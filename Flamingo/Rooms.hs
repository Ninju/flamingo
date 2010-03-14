module Flamingo.Rooms (Room(Room), exits, description,
                       Direction(North, East, South, West),
                       startingRoom, crampedCloset) where
import Data.Char (toLower)
import Data.List (intercalate)

data Direction     = North | East | South | West deriving (Eq, Show, Enum)
newtype Inhabitant = Name String
newtype RoomID     = RoomID String
data Room          = Room { exits :: [(Direction, Room)], description :: String, inhabitants :: [Inhabitant] }

startingRoom :: Room
startingRoom = Room { exits = [(North, crampedCloset)],
                      description = "You find yourself in a round room with a pillar in the middle.",
                      inhabitants = [] }

crampedCloset :: Room
crampedCloset = Room { exits = [(South, startingRoom)],
                       description = "You are in a cramped closet.",
                       inhabitants = [] }

instance Show Room where
  show r = description r ++ "\nExits: (" ++ intercalate ", " (map (map toLower . show . fst) $ exits r) ++ ")"
