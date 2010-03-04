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

startingRoom :: Room
startingRoom = Room { exits = [("north", crampedCloset)], description = "You find yourself in a round room with a pillar in the middle." }

crampedCloset :: Room
crampedCloset = Room { exits = [("south",startingRoom)], description = "You are in a cramped closet." }

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

--handleClient = do lookAtCurrentRoom
--                  print newLine
--                  print prompt
--                  env <- handle input
--
--handleClient :: ReaderT Environment IO ()
--handleClient = do h   <- asks (handle . connection)
--                  env <- ask
--                  tvR <- asks currentRoom
--                  liftIO $ do (atomically $ readTVar tvR) >>= hPutStrLn h . (++ "\n") . show
--                              forever $ do hPutStr h prompt
--                                           hFlush h
--                                           input <- hGetLine h
--                                           response <- runReaderT (execute input) env
--                                           hPutStrLn h response
--
run = bracket (listenOn $ PortNumber portNumber) sClose acceptConnections
