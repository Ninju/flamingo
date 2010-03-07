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

hGetPlayerName :: Handle -> IO String
hGetPlayerName h = do hPutStr h "What is your name? " >> hFlush h
                      n <- hGetLine h
                      if null (words n)
                        then hPutStrLn h "Name cannot be blank." >> hGetPlayerName h
                        else return n

acceptConnections :: Socket -> IO b
acceptConnections socket = do connection <- accept socket
                              room <- atomically $ startingRoom
                              let env = Env { connection = connection, currentRoom = room }
                              forkIO $ (runReaderT handleClient env `finally` hClose (handle connection))
                              acceptConnections socket

handleInput :: ReaderT Environment IO ()
handleInput = do h <- asks (handle . connection)
                 input <- liftIO $ do hDisplayPrompt h
                                      hGetLine h
                 env <- execute input
                 local (const env) handleInput

handleClient :: ReaderT Environment IO ()
handleClient = do h <- asks (handle . connection)
                  n <- liftIO $ hGetPlayerName h
                  r <- asks currentRoom
                  liftIO $ addInhabitant r n
                  (liftIO . hPutStrLn h . (++ "\n") . show) r
                  handleInput

run = bracket (listenOn $ PortNumber portNumber) sClose acceptConnections
