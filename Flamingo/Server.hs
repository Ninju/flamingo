module Flamingo.Server where
import Network
import System.IO
import Control.Concurrent
import Control.Exception hiding (handle)

type Connection = (Handle, String, PortNumber)

portNumber :: PortNumber
portNumber = 3333

handle :: Connection -> Handle
handle (h, _, _) = h

run = bracket (listenOn $ PortNumber portNumber) sClose setupAndAcceptConnections

setupAndAcceptConnections = acceptConnections

acceptConnections socket = do connection <- accept socket
                              forkIO $ (handleClient connection `finally` hClose (handle connection))
                              acceptConnections socket

handleClient connection@(handle,_,_) = do hFlush handle
                                          input <- hGetLine handle
                                          hPutStrLn handle input
                                          handleClient connection
