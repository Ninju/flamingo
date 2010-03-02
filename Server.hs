module Server where
import Network
import System.IO
import Control.Concurrent
import Control.Exception hiding (handle)

-- handle in Control.Exception is (flip catch)

type Connection = (Handle, String, PortNumber)

portNumber :: PortNumber
portNumber = 3333

prompt :: String
prompt = "> "

handle :: Connection -> Handle
handle (h, _, _) = h

run = bracket (listenOn $ PortNumber portNumber) sClose setupAndAcceptConnections

setupAndAcceptConnections = acceptConnections

acceptConnections socket = do connection <- accept socket
                              forkIO $ (handleClient connection `finally` hClose (handle connection))
                              acceptConnections socket

handleClient connection@(handle,_,_) = do hPutStr handle prompt
                                          hFlush handle
                                          input <- hGetLine handle
                                          if isCommand input
                                            then handleCommand connection input
                                            else handleInput connection input
                                          handleClient connection

isCommand :: String -> Bool
isCommand command = False

handleCommand :: Connection -> String -> IO ()
handleCommand connection command = return ()

handleInput :: Connection -> String -> IO ()
handleInput connection input = hPutStrLn (handle connection) input
