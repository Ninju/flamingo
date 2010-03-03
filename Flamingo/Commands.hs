module Flamingo.Commands where
import System.IO
import Control.Concurrent.STM
import Time (getClockTime, toUTCTime, calendarTimeToString)
import Control.Monad.Reader
import Flamingo.Utils

currentTime :: IO String
currentTime = getClockTime >>= return . calendarTimeToString . toUTCTime

command :: [String] -> ReaderT Environment IO String
command ("time":_) = liftIO $ currentTime
command ("look":_) = return "You see an empty room, waiting to be filled."
command _          = return "Invalid command"

execute :: String -> ReaderT Environment IO String
execute = command . words
