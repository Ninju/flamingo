module Flamingo.Commands where
import Control.Concurrent.STM
import Control.Monad.Reader
import System.IO
import Time (getClockTime, toUTCTime, calendarTimeToString)
import Flamingo.Rooms
import Flamingo.Utils

currentTime :: IO String
currentTime = getClockTime >>= return . calendarTimeToString . toUTCTime

move :: Direction -> Room -> ReaderT Environment IO ()
move direction room = ask >>= \env -> lift $ maybe (return ()) (f env) $ lookup direction (exits room)
                      where
                      f e room = runReaderT (setCurrentRoom room) e

command :: [String] -> ReaderT Environment IO String
command ("time":_)   = liftIO $ currentTime
command ("look":_)   = return "You see an empty room, waiting to be filled."
command ("move":[])  = return "Enter a direction in which to move."
command ("move":d:_) = do tvR <- asks currentRoom
                          r   <- lift $ atomically $ readTVar tvR
                          if isExit d r
                            then do move d r
                                    return "You have moved."
                            else return "You can't go that way."
command _            = return "Invalid command"

execute :: String -> ReaderT Environment IO String
execute = command . words
