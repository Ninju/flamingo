module Flamingo.Commands (execute) where
import Control.Arrow ((>>>))
import Control.Monad.Reader (ReaderT, asks, ask, local)
import Flamingo.Rooms (Direction(North, East, South, West), exits, moveInhabitant, inhabitants)
import Flamingo.Utils (Environment, mPutStrLn, currentRoom, modifyRooms, inhabitant, asksM, uCurrentRoom)

toDirection :: String -> Maybe Direction
toDirection d = lookup d $ zip ["north", "east", "south", "west"] [North .. West]

--move direction :: Direction -> ReaderT Environment IO Environment
move direction = do currentR <- asksM uCurrentRoom
                    case lookup direction (exits currentR) of
                      Nothing -> mPutStrLn "You can't move that way." >> ask
                      Just eR -> do i <- asks inhabitant
                                    modifyRooms (moveInhabitant i currentR eR)
                                    env <- ask
                                    let env' = env { currentRoom = eR }
                                    local (const env') look
                                    return env'

look :: ReaderT Environment IO ()
look = asksM uCurrentRoom >>= mPutStrLn . (++ "\n") . show

peeps :: ReaderT Environment IO ()
peeps = do currentR <- asksM uCurrentRoom
           mPutStrLn $ show (inhabitants currentR)


command :: [String] -> ReaderT Environment IO Environment
command ("look":_)    = look >> ask
command ("move":[])   = mPutStrLn "Enter a direction in which to move." >> ask
command ("move":d:_)  = maybe (mPutStrLn "You can't move that way." >> ask) move (toDirection d)
command ("peeps":_)   = peeps >> ask
command (d:_)         = maybe (command []) move (toDirection d)
command _             = mPutStrLn "Invalid command" >> ask

execute :: String -> ReaderT Environment IO Environment
execute = command . words
