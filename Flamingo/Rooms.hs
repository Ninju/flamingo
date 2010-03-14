module Flamingo.Rooms where
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.List
import System.IO
import Flamingo.Utils

data Environment = Env { connection :: Connection, currentRoom :: Room }
type Direction = String
data Room = Room { exits :: [(Direction, Room)], description :: String }

startingRoom :: Room
startingRoom = Room { exits = [("north", crampedCloset)], description = "You find yourself in a round room with a pillar in the middle." }

crampedCloset :: Room
crampedCloset = Room { exits = [("south",startingRoom)], description = "You are in a cramped closet." }

instance Show Room where
  show r = description r ++ "\nExits: (" ++ intercalate ", " (map fst $ exits r) ++ ")"

mIO :: (Handle -> IO a) -> ReaderT Environment IO a
mIO f = do h <- asks (handle . connection)
           liftIO (f h)

mPutStrLn :: String -> ReaderT Environment IO ()
mPutStrLn = mIO . flip hPutStrLn

mDisplayPrompt :: ReaderT Environment IO ()
mDisplayPrompt = mIO hDisplayPrompt

mGetLine :: ReaderT Environment IO String
mGetLine = mIO hGetLine

