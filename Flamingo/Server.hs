module Flamingo.Server where
import Network
import System.IO
import Control.Concurrent
import Control.Exception hiding (handle)

prompt :: String
prompt = "> "

type Connection = (Handle, String, PortNumber)

portNumber :: PortNumber
portNumber = 3333

handle :: Connection -> Handle
handle (h, _, _) = h

setupAndAcceptConnections :: Socket -> IO b
setupAndAcceptConnections = acceptConnections

acceptConnections :: Socket -> IO b
acceptConnections socket = do connection <- accept socket
                              forkIO $ (handleClient connection `finally` hClose (handle connection))
                              acceptConnections socket

handleClient :: (Handle, t, t1) -> IO b
handleClient connection@(handle,_,_) = do hPutStr handle prompt
                                          hFlush handle
                                          input <- hGetLine handle
                                          hPutStrLn handle input
                                          handleClient connection

run = bracket (listenOn $ PortNumber portNumber) sClose setupAndAcceptConnections
