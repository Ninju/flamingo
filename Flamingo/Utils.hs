module Flamingo.Utils where
import Network
import Control.Concurrent.STM
import Control.Monad.Reader
import System.IO (Handle)

type Connection = (Handle, String, PortNumber)

handle :: (Handle, a, b) -> Handle
handle (h,_,_) = h

