module Flamingo.Server where
import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Flamingo.Commands (execute)

prompt :: String
prompt = "> "

portNumber :: PortNumber
portNumber = 3333

handle :: (Handle, a, b) -> Handle
handle (h, _, _) = h

setupAndAcceptConnections :: Socket -> IO b
setupAndAcceptConnections socket = do currentRoom <- newTVarIO "start"
                                      acceptConnections socket currentRoom

acceptConnections :: Socket -> TVar a -> IO b
acceptConnections socket currentRoom = do connection <- accept socket
                                          forkIO $ (handleClient connection currentRoom `finally` hClose (handle connection))
                                          acceptConnections socket currentRoom

handleClient :: (Handle, a, b) -> TVar c -> IO d
handleClient connection@(handle,_,_) currentRoom = do hPutStr handle prompt
                                                      hFlush handle
                                                      input <- hGetLine handle
                                                      execute connection currentRoom input
                                                      handleClient connection currentRoom

run = bracket (listenOn $ PortNumber portNumber) sClose setupAndAcceptConnections
