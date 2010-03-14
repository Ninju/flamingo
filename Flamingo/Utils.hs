module Flamingo.Utils (Environment(Env), currentRoom, connection,
                       Connection,
                       handle, prompt, mPutStrLn, mIO, mDisplayPrompt, mGetLine, hDisplayPrompt, (<&>))  where
import Control.Arrow
import Control.Monad.Reader
import Network (PortNumber)
import System.IO
import Flamingo.Rooms (Room(Room))

type Connection = (Handle, String, PortNumber)
data Environment = Env { connection :: Connection, currentRoom :: Room }

handle :: (Handle, a, b) -> Handle
handle (h,_,_) = h

prompt :: String
prompt = "> "

hDisplayPrompt :: Handle -> IO ()
hDisplayPrompt h = hPutStr h prompt >> hFlush h

(<&>) :: Monad m => (a -> m b) -> (a -> m c) -> (a -> m (b,c))
(<&>) f g = runKleisli $ Kleisli f &&& Kleisli g

mIO :: (Handle -> IO a) -> ReaderT Environment IO a
mIO f = asks (handle . connection) >>= liftIO . f

mPutStrLn :: String -> ReaderT Environment IO ()
mPutStrLn = mIO . flip hPutStrLn

mDisplayPrompt :: ReaderT Environment IO ()
mDisplayPrompt = mIO hDisplayPrompt

mGetLine :: ReaderT Environment IO String
mGetLine = mIO hGetLine
