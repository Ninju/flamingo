module Flamingo.Rooms where
import Control.Concurrent.STM
import Control.Monad.Reader
import Flamingo.Utils

data Environment = Env { connection :: Connection, currentRoom :: TVar Room }
type Direction = String
data Room = Room { exits :: [(Direction, Room)], description :: String }

isExit :: Direction -> Room -> Bool
isExit direction room = False

setCurrentRoom :: Room -> ReaderT Environment IO ()
setCurrentRoom room = do current <- asks currentRoom
                         env     <- ask
                         lift $ atomically $ writeTVar current room
