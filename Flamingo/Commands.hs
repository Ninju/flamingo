module Flamingo.Commands where
import Time (getClockTime, toUTCTime, calendarTimeToString)

currentTime :: IO String
currentTime = do t <- getClockTime
                 return $ calendarTimeToString (toUTCTime t)

command :: [String] -> IO String
command ("time":_) = currentTime
command ("look":_) = return "You see an empty room, waiting to be filled."
command _          = return "Invalid command"

execute :: String -> IO String
execute = command . words
