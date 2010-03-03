module Flamingo.Rooms where
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.List
import Flamingo.Utils

data Environment = Env { connection :: Connection, currentRoom :: TVar Room }
type Direction = String
data Room = Room { exits :: [(Direction, Room)], description :: String }

instance Show Room where
  show r = description r ++ "\nExits: (" ++ intercalate ", " (map fst $ exits r) ++ ")"


isExit :: Direction -> Room -> Bool
isExit direction room = case lookup direction (exits room) of
                          Nothing -> False
                          Just _  -> True

setCurrentRoom :: Room -> ReaderT Environment IO Room
setCurrentRoom room = do current <- asks currentRoom
                         env     <- ask
                         lift $ atomically $ writeTVar current room >> readTVar current
