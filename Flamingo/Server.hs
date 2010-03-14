module Flamingo.Server where
import Network
import System.IO
import Control.Arrow
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

(<&>) :: Monad m => (a -> m b) -> (a -> m c) -> (a -> m (b,c))
(<&>) f g = runKleisli $ Kleisli f &&& Kleisli g

mIO :: (Handle -> IO a) -> ReaderT Environment IO a
mIO f = do h <- asks (handle . connection)
           liftIO (f h)

mPutStrLn :: String -> ReaderT Environment IO ()
mPutStrLn = mIO . flip hPutStrLn

mDisplayPrompt :: ReaderT Environment IO ()
mDisplayPrompt = mIO hDisplayPrompt

mGetLine :: ReaderT Environment IO String
mGetLine = mIO hGetLine

hDisplayPrompt :: Handle -> IO ()
hDisplayPrompt h = hPutStr h prompt >> hFlush h

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
