module Flamingo.Commands where
import System.IO
import Control.Concurrent.STM
import Time (getClockTime, toUTCTime, calendarTimeToString)
import Flamingo.Rooms (description, move, isExit)

currentTime :: IO String
currentTime = do t <- getClockTime
                 return $ calendarTimeToString (toUTCTime t)

command :: [String] -> IO String
command _ _ ("time":_) = currentTime
command _ _ ("look":_) = return "You see an empty room, waiting to be filled."
command _ r ("move":d) = if isExit d r
                           then do move d r
                                   return (description r)
                           else return "You can't go that way."
command _ _ _          = return "Invalid command"

execute :: (Handle, a, b) -> TVar c -> String -> IO ()
execute connection@(handle,_,_) currentRoom input = command connection currentRoom input >>= hPutStrLn handle
