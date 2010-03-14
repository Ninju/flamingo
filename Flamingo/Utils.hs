module Flamingo.Utils (Environment(Env), currentRoomID, currentRoom, connection, tvRooms, inhabitant,
                       Connection,
                       modifyCurrentRoom, modifyRooms, modifyRoom, getRoom, asksM,
                       handle, prompt, mGetName, mPutStrLn, mIO, mDisplayPrompt, mGetLine, hDisplayPrompt, (<&>))  where
import Control.Arrow (Kleisli(Kleisli), runKleisli, (&&&))
import Control.Concurrent.STM (TVar, atomically, writeTVar, readTVar)
import Control.Monad.Reader (ReaderT, asks, liftIO, ask, lift)
import Data.List (delete, find)
import Data.Maybe (fromJust)
import Network (PortNumber)
import System.IO (Handle, hFlush, hPutStr, hPutStrLn, hGetLine)
import Flamingo.Rooms (Room(Room), RoomID(RoomID), Inhabitant, updateRoom, roomID)

type Connection = (Handle, String, PortNumber)
data Environment = Env { connection :: Connection, currentRoomID :: RoomID, tvRooms :: TVar [Room], inhabitant :: Inhabitant }

handle :: (Handle, a, b) -> Handle
handle (h,_,_) = h

prompt :: String
prompt = "> "

hDisplayPrompt :: Handle -> IO ()
hDisplayPrompt h = hPutStr h prompt >> hFlush h

hGetName :: Handle -> IO String
hGetName h = do hPutStrLn h "What is your name?"
                hDisplayPrompt h
                name <- hGetLine h
                case words name of
                  []  -> hPutStrLn h "Name must not be blank." >> hGetName h
                  [n] -> return n
                  _   -> hPutStrLn h "Name must not contain spaces." >> hGetName h

currentRoom :: Environment -> IO Room
currentRoom env = findRoom ((currentRoomID env ==) . roomID) env

findRoom :: (Room -> Bool) -> Environment -> IO Room
findRoom f env = do rs <- atomically $ readTVar $ tvRooms env
                    return $ fromJust (find f rs)

getRoom :: Room -> Environment -> IO Room
getRoom r env = findRoom (== r) env

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

mGetName :: ReaderT Environment IO String
mGetName = mIO hGetName

modifyRooms :: ([Room] -> [Room]) -> ReaderT Environment IO ()
modifyRooms f = do tvRs <- asks tvRooms
                   (liftIO . atomically) (readTVar tvRs >>= writeTVar tvRs . f)

modifyRoom :: (Room -> Room) -> Room -> ReaderT Environment IO ()
modifyRoom f r = modifyRooms (updateRoom f r)

modifyCurrentRoom :: (Room -> Room) -> ReaderT Environment IO ()
modifyCurrentRoom f = asksM currentRoom >>= modifyRoom f
