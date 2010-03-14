module Flamingo.Server (run) where
import Network (PortNumber, PortID(PortNumber), accept, listenOn, sClose)
import System.IO (hClose, hGetLine)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVar, atomically)
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import Control.Exception (bracket, finally)
import Flamingo.Commands (execute)
import Flamingo.Utils (Environment(Env), inhabitant, tvRooms, currentRoom, connection, handle, mPutStrLn, hDisplayPrompt, mIO, (<&>), modifyRoom)
import Flamingo.Rooms (startingRoom, crampedCloset, Inhabitant(Name), addInhabitant)

portNumber :: PortNumber
portNumber = 3333

setupAndAcceptConnections socket = do tvRs <- atomically $ newTVar [startingRoom, crampedCloset]
                                      acceptConnections socket tvRs

acceptConnections socket tvRs = do connection <- accept socket
                                   let env = Env { connection = connection, currentRoom = startingRoom, tvRooms = tvRs, inhabitant = Name "" }
                                   forkIO $ (runReaderT handleClient env `finally` hClose (handle connection))
                                   acceptConnections socket tvRs

handleClient :: ReaderT Environment IO ()
handleClient = do r <- asks currentRoom
                  i <- asks inhabitant
                  modifyRoom (addInhabitant i) r
                  mPutStrLn (show r ++ "\n")
                  handleInput

handleInput :: ReaderT Environment IO ()
handleInput = do (_, input) <- mIO (hDisplayPrompt <&> hGetLine)
                 env <- execute input
                 local (const env) handleInput

run = bracket (listenOn $ PortNumber portNumber) sClose setupAndAcceptConnections
