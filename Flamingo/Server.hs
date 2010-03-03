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

prompt :: String
prompt = "> "

portNumber :: PortNumber
portNumber = 3333

setupAndAcceptConnections :: Socket -> IO b
setupAndAcceptConnections socket = do currentRoom <- newTVarIO "start"
                                      acceptConnections socket currentRoom

acceptConnections socket currentRoom = do connection <- accept socket
                                          let env = Env { connection = connection, currentRoom = currentRoom }
                                          forkIO $ (runReaderT handleClient env `finally` hClose (handle connection))
                                          acceptConnections socket currentRoom

handleClient :: ReaderT Environment IO ()
handleClient = do h   <- asks (handle . connection)
                  env <- ask
                  liftIO $ forever $ do hPutStr h prompt
                                        hFlush h
                                        input <- hGetLine h
                                        runReaderT (execute input) env

run = bracket (listenOn $ PortNumber portNumber) sClose setupAndAcceptConnections
