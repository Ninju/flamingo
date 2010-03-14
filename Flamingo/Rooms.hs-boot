module Flamingo.Rooms where
  data Direction = North | East | South | West
  data Room = Room { exits :: [(Direction, Room)], description :: String }
