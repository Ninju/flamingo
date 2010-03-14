module Flamingo.Commands where
import Control.Concurrent.STM
import Control.Monad.Reader
import System.IO
import Flamingo.Rooms
import Flamingo.Utils

isDirection :: Direction -> Bool
isDirection = flip elem ["north", "east", "south", "west"]

move :: Direction -> ReaderT Environment IO Environment
move direction = do current <- asks currentRoom
                    case lookup direction (exits current) of
                      Nothing -> do mPutStrLn "You can't move that way."
                                    ask
                      Just r  -> do env <- ask
                                    let newEnv = env { currentRoom = r }
                                    local (const newEnv) look
                                    return newEnv

look :: ReaderT Environment IO ()
look = do r <- asks currentRoom
          mPutStrLn (show r ++ "\n")


command :: [String] -> ReaderT Environment IO Environment
command ("look":_)    = look >> ask
command ("move":[])   = (asks (handle . connection) >>= liftIO . (flip hPutStrLn "Enter a direction in which to move.")) >> ask
command ("move":d:_)  = move d
command (d:_)         = if isDirection d then move d else command []
command _             = (asks (handle . connection) >>= liftIO . (flip hPutStrLn "Invalid command")) >> ask

execute :: String -> ReaderT Environment IO Environment
execute = command . words
