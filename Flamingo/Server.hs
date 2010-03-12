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
import Flamingo.Rooms

prompt :: String
prompt = "> "

portNumber :: PortNumber
portNumber = 3333

hDisplayPrompt :: Handle -> IO ()
hDisplayPrompt h = hPutStr h prompt >> hFlush h

acceptConnections socket = do connection <- accept socket
                              let env = Env { connection = connection, currentRoom = startingRoom }
                              forkIO $ (runReaderT handleClient env `finally` hClose (handle connection))
                              acceptConnections socket

handleClient :: ReaderT Environment IO ()
handleClient = do h <- asks (handle . connection)
                  r <- asks currentRoom
                  (liftIO . hPutStrLn h . (++ "\n") . show) r
                  handleInput

handleInput :: ReaderT Environment IO ()
handleInput = do h <- asks (handle . connection)
                 input <- liftIO $ do hDisplayPrompt h
                                      hGetLine h
                 env <- execute input
                 local (const env) handleInput

run = bracket (listenOn $ PortNumber portNumber) sClose acceptConnections
