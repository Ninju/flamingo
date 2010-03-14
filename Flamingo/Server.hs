module Flamingo.Server (run) where
import Network (PortNumber, PortID(PortNumber), accept, listenOn, sClose)
import System.IO (hClose, hGetLine, hPutStrLn)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVar, atomically)
import Control.Monad.Reader (ReaderT, runReaderT, asks, local, ask)
import Control.Exception (bracket, finally)
import Flamingo.Commands (execute)
import Flamingo.Utils (Environment(Env), inhabitant, tvRooms, currentRoom, connection, handle, mPutStrLn, hDisplayPrompt, mIO, (<&>), modifyRoom, asksM, uCurrentRoom)
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
handleClient = do (_,name) <- mIO (flip hPutStrLn "What is your name?" <&> hDisplayPrompt <&> hGetLine)
                  e            <- ask
                  let env = e { inhabitant = Name name }
                  r <- asksM uCurrentRoom
                  modifyRoom (addInhabitant (Name name)) r
                  r' <- asksM uCurrentRoom
                  mPutStrLn (show r' ++ "\n")
                  local (const env) handleInput

handleInput :: ReaderT Environment IO ()
handleInput = do (_, input) <- mIO (hDisplayPrompt <&> hGetLine)
                 env <- execute input
                 local (const env) handleInput

run = bracket (listenOn $ PortNumber portNumber) sClose setupAndAcceptConnections
