module Flamingo.Server (run) where
import Network (PortNumber, PortID(PortNumber), accept, listenOn, sClose)
import System.IO (hClose, hGetLine, hPutStrLn)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVar, atomically)
import Control.Monad.Reader (ReaderT, runReaderT, asks, local, ask)
import Control.Exception (bracket, finally)
import Flamingo.Commands (execute, look)
import Flamingo.Utils (Environment(Env), inhabitant, tvRooms, currentRoomID, currentRoom, connection, handle, mGetName, mPutStrLn, hDisplayPrompt, mIO, (<&>), modifyRoom, asksM, modifyCurrentRoom)
import Flamingo.Rooms (startingRoom, crampedCloset, Inhabitant(Name), addInhabitant, roomID)

portNumber :: PortNumber
portNumber = 3333

setupAndAcceptConnections socket = do tvRs <- atomically $ newTVar [startingRoom, crampedCloset]
                                      acceptConnections socket tvRs

acceptConnections socket tvRs = do connection <- accept socket
                                   let env = Env { connection = connection, currentRoomID = roomID startingRoom, tvRooms = tvRs, inhabitant = Name "" }
                                   forkIO $ (runReaderT handleClient env `finally` hClose (handle connection))
                                   acceptConnections socket tvRs

handleClient :: ReaderT Environment IO ()
handleClient = do name <- mGetName
                  env  <- ask
                  let newEnv = env { inhabitant = Name name }
                  modifyCurrentRoom (addInhabitant (Name name))
                  local (const newEnv) look
                  local (const newEnv) handleInput

handleInput :: ReaderT Environment IO ()
handleInput = do (_, input) <- mIO (hDisplayPrompt <&> hGetLine)
                 env <- execute input
                 local (const env) handleInput

run = bracket (listenOn $ PortNumber portNumber) sClose setupAndAcceptConnections
