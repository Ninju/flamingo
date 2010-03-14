module Flamingo.Utils where
import Control.Arrow
import Control.Concurrent.STM
import Control.Monad.Reader
import Network
import System.IO

type Connection = (Handle, String, PortNumber)

handle :: (Handle, a, b) -> Handle
handle (h,_,_) = h

prompt :: String
prompt = "> "

hDisplayPrompt :: Handle -> IO ()
hDisplayPrompt h = hPutStr h prompt >> hFlush h

(<&>) :: Monad m => (a -> m b) -> (a -> m c) -> (a -> m (b,c))
(<&>) f g = runKleisli $ Kleisli f &&& Kleisli g
