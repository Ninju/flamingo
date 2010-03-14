module Flamingo.Rooms (Room(Room), exits, description, roomID, inhabitants,
                       RoomID(RoomID),
                       Direction(North, East, South, West),
                       Inhabitant(Name),
                       updateRoom, startingRoom, crampedCloset, moveInhabitant, addInhabitant) where
import Data.Char (toLower)
import Data.List (intercalate, delete)

data Direction     = North | East | South | West deriving (Eq, Show, Enum)
newtype Inhabitant = Name String deriving (Eq, Show)
newtype RoomID     = RoomID String deriving Eq
data Room          = Room { exits :: [(Direction, Room)], description :: String, inhabitants :: [Inhabitant], roomID :: RoomID }

instance Eq Room where
  r == r' = roomID r == roomID r'

startingRoom :: Room
startingRoom = Room { roomID      = RoomID "start",
                      exits       = [(North, crampedCloset)],
                      description = "You find yourself in a round room with a pillar in the middle.",
                      inhabitants = [] }

crampedCloset :: Room
crampedCloset = Room { roomID      = RoomID "closet",
                       exits       = [(South, startingRoom)],
                       description = "You are in a cramped closet.",
                       inhabitants = [] }

instance Show Room where
  show r = description r ++ "\nExits: (" ++ intercalate ", " (map (map toLower . show . fst) $ exits r) ++ ")"

moveInhabitant :: Inhabitant -> Room -> Room -> [Room] -> [Room]
moveInhabitant i from target = updateRoom (removeInhabitant i) from . updateRoom (addInhabitant i) target

updateRoom :: (Room -> Room) -> Room -> [Room] -> [Room]
updateRoom f r = (f r:) . delete r

modifyInhabitants :: ([Inhabitant] -> [Inhabitant]) -> Room -> Room
modifyInhabitants f room = room { inhabitants = f (inhabitants room) }

addInhabitant :: Inhabitant -> Room -> Room
addInhabitant i = modifyInhabitants (i:)

removeInhabitant :: Inhabitant -> Room -> Room
removeInhabitant i = modifyInhabitants (delete i)
