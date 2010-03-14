module Flamingo.Utils (Environment(Env), currentRoom, connection, tvRooms, inhabitant,
                       Connection,
                       getRoom, asksM, uCurrentRoom, modifyRooms, modifyRoom, replaceFirstWhere, handle, prompt, mPutStrLn, mIO, mDisplayPrompt, mGetLine, hDisplayPrompt, (<&>))  where
import Control.Arrow (Kleisli(Kleisli), runKleisli, (&&&))
import Control.Concurrent.STM (TVar, atomically, writeTVar, readTVar)
import Control.Monad.Reader (ReaderT, asks, liftIO, ask, lift)
import Data.List (delete)
import Network (PortNumber)
import System.IO (Handle, hFlush, hPutStr, hPutStrLn, hGetLine)
import {-# SOURCE #-} Flamingo.Rooms (Room(Room), RoomID(RoomID), Inhabitant)

type Connection = (Handle, String, PortNumber)
data Environment = Env { connection :: Connection, currentRoom :: Room, tvRooms :: TVar [Room], inhabitant :: Inhabitant }

handle :: (Handle, a, b) -> Handle
handle (h,_,_) = h

prompt :: String
prompt = "> "

hDisplayPrompt :: Handle -> IO ()
hDisplayPrompt h = hPutStr h prompt >> hFlush h

uCurrentRoom :: Environment -> IO Room
uCurrentRoom env = getRoom (currentRoom env) env

getRoom :: Room -> Environment -> IO Room
getRoom r env = do rs <- atomically $ readTVar $ tvRooms env
                   return $ head (dropWhile (not . (== r)) rs)

asksM f = ask >>= lift . f

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

modifyRooms :: ([Room] -> [Room]) -> ReaderT Environment IO ()
modifyRooms f = do tvRs <- asks tvRooms
                   (liftIO . atomically) (readTVar tvRs >>= writeTVar tvRs . f)


modifyRoom :: (Room -> Room) -> Room -> ReaderT Environment IO ()
modifyRoom f r = modifyRooms $ ((f r):) . (delete r)

replaceFirstWhere :: (a -> Bool) -> a -> [a] -> [a]
replaceFirstWhere p y []     = []
replaceFirstWhere p y (x:xs) = if p x then y : xs else x : replaceFirstWhere p y xs
