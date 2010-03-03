module Flamingo.Server where
import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Control.Exception hiding (handle)
import Flamingo.Commands (execute)
import Flamingo.Utils
import Flamingo.Rooms

prompt :: String
prompt = "> "

portNumber :: PortNumber
portNumber = 3333

startingRoom :: Room
startingRoom = Room { exits = [("north", crampedCloset)], description = "You find yourself in a round room with a pillar in the middle." }

crampedCloset :: Room
crampedCloset = Room { exits = [("south",startingRoom)], description = "You are in a cramped closet." }

setupAndAcceptConnections :: Socket -> IO b
setupAndAcceptConnections socket = do currentRoom <- newTVarIO startingRoom
                                      acceptConnections socket currentRoom

acceptConnections socket currentRoom = do connection <- accept socket
                                          let env = Env { connection = connection, currentRoom = currentRoom }
                                          forkIO $ (runReaderT handleClient env `finally` hClose (handle connection))
                                          acceptConnections socket currentRoom

handleClient :: ReaderT Environment IO ()
handleClient = do h   <- asks (handle . connection)
                  env <- ask
                  tvR <- asks currentRoom
                  liftIO $ forever $ do currentRoom <- atomically $ readTVar tvR
                                        hPutStrLn h (show currentRoom)
                                        hPutStr h prompt
                                        hFlush h
                                        input <- hGetLine h
                                        response <- runReaderT (execute input) env
                                        hPutStrLn h response

run = bracket (listenOn $ PortNumber portNumber) sClose setupAndAcceptConnections
