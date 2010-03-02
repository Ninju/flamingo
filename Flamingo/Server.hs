module Flamingo.Server where
import Network
import System.IO
import Control.Concurrent
import Control.Exception hiding (handle)
import Flamingo.Commands (execute)

type Connection = (Handle, String, PortNumber)

prompt :: String
prompt = "> "

portNumber :: PortNumber
portNumber = 3333

handle :: Connection -> Handle
handle (h, _, _) = h

setupAndAcceptConnections :: Socket -> IO b
setupAndAcceptConnections socket = do currentRoom <- newTVar "start"
                                      acceptConnections socket currentRoom

acceptConnections :: Socket -> IO b
acceptConnections socket currentRoom = do connection <- accept socket
                                          forkIO $ (handleClient connection currentRoom `finally` hClose (handle connection))
                                          acceptConnections socket

handleClient :: Connection -> IO a
handleClient connection@(handle,_,_) currentRoom = do hPutStr handle prompt
                                                      hFlush handle
                                                      input <- hGetLine handle
                                                      response <- (execute input)
                                                      hPutStrLn handle response
                                                      handleClient connection

run = bracket (listenOn $ PortNumber portNumber) sClose setupAndAcceptConnections
