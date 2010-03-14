module Flamingo.Commands (execute) where
import Control.Monad.Reader (ReaderT, asks, ask, local)
import Flamingo.Rooms (Direction(North, East, South, West), exits)
import Flamingo.Utils (Environment, mPutStrLn, currentRoom)

toDirection :: String -> Maybe Direction
toDirection d = lookup d $ zip ["north", "east", "south", "west"] [North .. West]

move :: Direction -> ReaderT Environment IO Environment
move direction = do current <- asks currentRoom
                    case lookup direction (exits current) of
                      Nothing -> mPutStrLn "You can't move that way." >> ask
                      Just r  -> do env <- ask
                                    let newEnv = env { currentRoom = r }
                                    local (const newEnv) look
                                    return newEnv

look :: ReaderT Environment IO ()
look = asks currentRoom >>= mPutStrLn . (++ "\n") . show

command :: [String] -> ReaderT Environment IO Environment
command ("look":_)    = look >> ask
command ("move":[])   = mPutStrLn "Enter a direction in which to move." >> ask
command ("move":d:_)  = maybe (mPutStrLn "You can't move that way." >> ask) move (toDirection d)
command (d:_)         = maybe (command []) move (toDirection d)
command _             = mPutStrLn "Invalid command" >> ask

execute :: String -> ReaderT Environment IO Environment
execute = command . words
