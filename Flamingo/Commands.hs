module Flamingo.Commands where
import System.IO
import Control.Concurrent.STM
import Time (getClockTime, toUTCTime, calendarTimeToString)

currentTime :: IO String
currentTime = do t <- getClockTime
                 return $ calendarTimeToString (toUTCTime t)

command :: [String] -> IO String
command ("time":_) = currentTime
command ("look":_) = return "You see an empty room, waiting to be filled."
command _          = return "Invalid command"

execute :: (Handle, a, b) -> TVar c -> String -> IO ()
execute (handle,_,_) _ input = (command . words) input >>= hPutStrLn handle
