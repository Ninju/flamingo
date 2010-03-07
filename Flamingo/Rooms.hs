module Flamingo.Rooms where
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.List
import Flamingo.Utils

data Environment = Env { connection :: Connection, currentRoom :: Room }

type Direction = String
type Inhabitant = String
data Room = Room { exits :: [(Direction, Room)], description :: String, inhabitants :: TVar [Inhabitant] }

startingRoom :: STM Room
startingRoom = do r <- crampedCloset
                  i <- newTVar []
                  return Room { exits = [("north", r)],
                                description = "You find yourself in a round room with a pillar in the middle.",
                                inhabitants = i }

crampedCloset :: STM Room
crampedCloset = do r <- startingRoom
                   i <- newTVar []
                   return Room { exits = [("south", r)],
                                 description = "You are in a cramped closet.",
                                 inhabitants = i }

instance Show Room where
  show r = description r ++ "\nExits: (" ++ intercalate ", " (map fst $ exits r) ++ ")"
--                                         ++ "\nInhabitants: (" ++ (readTVar (inhabitants r))  ++ ")"

addInhabitant :: Room -> Inhabitant -> IO [Inhabitant]
addInhabitant r n = atomically $ do i <- readTVar $ inhabitants r
                                    ; writeTVar (inhabitants r) $! (n : i)
                                    ; return i
