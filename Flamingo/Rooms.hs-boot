module Flamingo.Rooms (updateRoom) where
  data Direction = North | East | South | West
  newtype Inhabitant = Name String
  newtype RoomID = RoomID String
  data Room = Room { exits :: [(Direction, Room)], description :: String, inhabitants :: [Inhabitant], roomID :: RoomID }
  instance Eq Room
