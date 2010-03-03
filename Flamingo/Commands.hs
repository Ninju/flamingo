module Flamingo.Commands where
import Control.Concurrent.STM
import Control.Monad.Reader
import Flamingo.Rooms
import Flamingo.Utils

move :: Direction -> Room -> ReaderT Environment IO Room
move direction room = ask >>= \env -> lift $ maybe (return room) (flip runReaderT env . setCurrentRoom) $ lookup direction (exits room)

command :: [String] -> ReaderT Environment IO String
command ("look":_)   = asks currentRoom >>= lift . atomically . readTVar >>= return . (++ "\n") . show
command ("move":[])  = return "Enter a direction in which to move."
command ("move":d:_) = do tvR <- asks currentRoom
                          r   <- lift $ atomically $ readTVar tvR
                          if isExit d r
                            then move d r >>= return . (++ "\n") . show
                            else return "You can't go that way."
command (d:_)        = if elem d ["north", "south", "east", "west"]
                         then command ["move", d]
                         else return "Invalid command"
command _            = return "Invalid command"

execute :: String -> ReaderT Environment IO String
execute = command . words
