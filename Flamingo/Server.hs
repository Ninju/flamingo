module Flamingo.Server (run) where
import Network (PortNumber, PortID(PortNumber), accept, listenOn, sClose)
import System.IO (hClose, hGetLine)
import Control.Concurrent (forkIO)
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import Control.Exception (bracket, finally)
import Flamingo.Commands (execute)
import Flamingo.Utils (Environment(Env), handle, mPutStrLn, hDisplayPrompt, currentRoom, connection, mIO, (<&>))
import Flamingo.Rooms (startingRoom)

portNumber :: PortNumber
portNumber = 3333

acceptConnections socket = do connection <- accept socket
                              let env = Env { connection = connection, currentRoom = startingRoom }
                              forkIO $ (runReaderT handleClient env `finally` hClose (handle connection))
                              acceptConnections socket

handleClient :: ReaderT Environment IO ()
handleClient = do r <- asks currentRoom
                  mPutStrLn (show r ++ "\n")
                  handleInput

handleInput :: ReaderT Environment IO ()
handleInput = do (_, input) <- mIO (hDisplayPrompt <&> hGetLine)
                 env <- execute input
                 local (const env) handleInput

run = bracket (listenOn $ PortNumber portNumber) sClose acceptConnections
