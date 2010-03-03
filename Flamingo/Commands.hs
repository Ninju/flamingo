module Flamingo.Commands where
import System.IO
import Control.Concurrent.STM
import Time (getClockTime, toUTCTime, calendarTimeToString)
import Control.Monad.Reader
import Flamingo.Utils

currentTime :: IO String
currentTime = do t <- getClockTime
                 return $ calendarTimeToString (toUTCTime t)

command :: [String] -> ReaderT Environment IO String
command ("time":_) = liftIO $ currentTime
command ("look":_) = return "You see an empty room, waiting to be filled."
command _          = return "Invalid command"

execute :: String -> ReaderT Environment IO ()
execute input = do h   <- asks (handle . connection)
                   env <- ask
                   let output = runReaderT (command (words input)) env
                   liftIO $ hPutStrLn h =<< output
