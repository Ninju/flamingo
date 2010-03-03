module Flamingo.Utils where
import Network
import Control.Concurrent.STM (TVar)
import System.IO (Handle)

data Environment = Env { connection :: Connection, currentRoom :: TVar String }
type Connection = (Handle, String, PortNumber)

handle :: (Handle, a, b) -> Handle
handle (h,_,_) = h
