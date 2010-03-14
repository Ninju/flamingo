module Flamingo.Rooms where
  type Direction = String
  data Room = Room { exits :: [(Direction, Room)], description :: String }
