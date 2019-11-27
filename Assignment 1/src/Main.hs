module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.Environment
import System.IO
import Data.IORef
import Data.ByteString.Char8           ( ByteString )
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import Data.Word
import Data.List (elemIndex, (\\) )
import Data.Maybe (fromMaybe)
import Crypto.Hash.SHA1


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let config = parseConfig args

  -- Do stuff here
 -- putStrLn $ "I'm in program mode " ++ show (cfgMode config) ++ " with lock " ++ show (cfgSync config) ++ ","
 -- putStrLn $ "performing the " ++ show (cfgModulus config) ++ "-test with " ++ show (cfgThreads config) ++ " threads"
 -- putStrLn $ "on the numbers " ++ show (cfgLower config) ++ " .. " ++ show (cfgUpper config) ++ "."

  let ints = [(cfgLower config)..(cfgUpper config)]
  
  case cfgMode config of
    Count -> case cfgSync config of
      SyncMVar -> do countMVar (cfgThreads config) ints (cfgModulus config)
      SyncIORef -> do countIORef (cfgThreads config) ints (cfgModulus config)
      
    List  -> case cfgSync config of
      SyncMVar -> do 
        foo <- async $ mVarList (cfgThreads config) ints (cfgModulus config)
        wait foo
      SyncIORef -> iORefList (cfgThreads config) ints (cfgModulus config)
    Search expected
      | checkHash expected 274856182 -> putStrLn "Given hash matches with account number 274856182."
      | otherwise                    -> putStrLn "Hash does not match with account number 274856182."
    _ -> return ()

  -- forkIO $ replicateM_ 100 (putChar 'A')
  -- forkIO $ replicateM_ 100 (putChar 'B')

  threadDelay 100000

-- Parses the command line arguments
parseConfig :: [String] -> Config
parseConfig (sync' : b : e : m : threads : mode' : rest)
  = Config sync mode (read b) (read e) (read m) (read threads)
  where
    -- Synchronization method
    sync = case sync' of
      "ioref" -> SyncIORef
      "mvar" -> SyncMVar
      _ -> error "Illegal sync method"
    -- Program mode
    mode = case (mode', rest) of
      ("count", []) -> Count
      ("list", []) -> List
      ("search", [q]) -> Search $ readHash q
      _ -> error "Illegal mode or wrong number of arguments"
parseConfig _ = error "Wrong number of arguments"

data Sync = SyncMVar | SyncIORef deriving Show
data Mode = Count | List | Search ByteString deriving Show

data Config = Config { cfgSync :: !Sync, cfgMode :: !Mode, cfgLower :: !Int, cfgUpper :: !Int, cfgModulus :: !Int, cfgThreads :: !Int } deriving Show

-- Reads a hash passed as command line argument.
readHash :: String -> ByteString
readHash = B.pack . readHash'
 
-- Two hexadecimal characters become one byte. Input size must thus be even.
readHash' :: String -> [Word8]
readHash' [] = []
readHash' [_] = error "Illegal hexadecimal hash"
readHash' (c1:c2:cs) = v1 * 16 + v2 : readHash' cs
  where
    v1 = value c1
    v2 = value c2
    value c = fromIntegral $ fromMaybe (error "Illegal hexadecimal hash") $ readHexadecimal c

readHexadecimal :: Char -> Maybe Int
readHexadecimal c = c `elemIndex` (['0'..'9'] ++ ['a'..'f'])

-- Checks if a number matches with the specified hash
checkHash :: ByteString -> Int -> Bool
checkHash expected value = expected == hash (B8.pack $ show value)

data Lock = Unlocked | Locked deriving Eq
type IOLock = IORef Lock

-- Guard some action using the given lock
--
interlocked :: IOLock -> IO a -> IO ()
interlocked lock ac = do
  access <- atomCAS lock Unlocked Locked
  if access
    then do
      _ <- ac
      atomicModifyIORef' lock (\_ -> (Unlocked, ())) 
  else
    interlocked lock ac


atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
    atomicModifyIORef' ptr (\ cur -> if cur == old
                                    then (new, True)
                                    else (cur, False))


countIORef :: Int -> [Int] -> Int -> IO ()
countIORef threads list modulo = do
  lock <- newIORef Unlocked
  counter <- newIORef 0 
  makeForkIORef counter lock threads list modulo
  threadDelay 1000
  c <- readIORef counter
  putStrLn (show c)

makeForkIORef :: IORef Int -> IORef Lock -> Int -> [Int] -> Int -> IO ()
makeForkIORef _  _ 0 _ _ = return ()
makeForkIORef c lock 1 ints modulo  = do
  _ <- forkIO $ do
    let count = countMode1 ints modulo
    interlocked lock (action c count)
  return ()
makeForkIORef c lock n ints modulo = do
  _ <- forkIO $ do
    let count = countMode (getListPart n ints) modulo
    interlocked lock (action c count)
  makeForkIORef c lock (n-1) (ints \\ (getListPart n ints)) modulo

action :: Num a => IORef a -> a -> IO ()
action c count = do 
  old <- readIORef c
  writeIORef c (old + count) 

countMVar :: Int -> [Int] -> Int -> IO ()
countMVar threads list modulo = do
  counter <- newMVar 0 
  makeForkMVar counter threads list modulo
  threadDelay 1000
  c <- takeMVar counter
  putStrLn (show c)

makeForkMVar :: MVar Int -> Int -> [Int] -> Int -> IO ()
makeForkMVar _ 0 _ _ = return ()
makeForkMVar c 1 ints modulo  = do
  _ <- forkIO $ do 
    let count = countMode1 ints modulo
    old <- takeMVar c
    putMVar c (old + count)
  return ()
makeForkMVar c n ints modulo  = do
  _ <- forkIO $ do
    let count = countMode (getListPart n ints) modulo
    old <- takeMVar c
    putMVar c (old + count)
  makeForkMVar c (n-1) (ints \\ (getListPart n ints)) modulo


--split list, get the first Nth part of the list  
getListPart :: Int -> [Int] -> [Int]
getListPart 1 list = list
getListPart n list = take (floor (1 / (fromIntegral n) * (fromIntegral $ length list))) list
 
--Mtest functions
digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

weights :: Int -> [Int]
weights n = reverse [1..(length (digs n))]

mtest :: Int -> Int -> Bool
mtest number m = mod (sum(zipWith (*) (digs number) (weights number))) m == 0

--mtest for every 1st thread
countMode1 :: [Int] -> Int -> Int
countMode1 [] _= 0
countMode1 l@(x:_) modulo = countMode [x..((last l)-1)] modulo  

--mtest for every Nth thread
countMode :: [Int] -> Int -> Int
countMode list modulo = length [x | x <- list, mtest x modulo]

iORefList :: Int -> [Int] -> Int -> IO ()
iORefList threads list modulo = do
  lock <- newIORef Unlocked
  iORefListFork threads list modulo lock
  return ()

iORefListFork :: Int -> [Int] -> Int -> IORef Lock -> IO ()
iORefListFork 0 _ _ _ = return ()
iORefListFork 1 ints modulo lock = do
  _ <- forkIO $ do 
    interlocked lock (listMode1IORef ints modulo)
  return ()
iORefListFork n ints modulo lock  = do
  _ <- forkIO $ do
    interlocked lock (listModeIORef (getListPart n ints) modulo)
  iORefListFork (n-1) (ints \\ (getListPart n ints)) modulo lock



--listmode
mVarList :: Int -> [Int] -> Int -> IO ()
mVarList threads list modulo = do
  writelock <- newMVar 1
  mVarListFork threads list modulo writelock
  return()

mVarListFork :: Int -> [Int] -> Int -> MVar Int -> IO ()
mVarListFork 0 _ _ _ = return ()
mVarListFork 1 ints modulo right = do
  _ <- forkIO $ do 
    listMode1 ints modulo right
  return ()
mVarListFork n ints modulo right  = do
  _ <- forkIO $ do
    listMode (getListPart n ints) modulo right
  mVarListFork (n-1) (ints \\ (getListPart n ints)) modulo right


--mtest for every Nth thread
listMode :: [Int] -> Int -> MVar Int -> IO()
listMode [] _ _ = return ()
listMode (x:xs) modulo right =  if mtest x modulo 
    then do
      v <- takeMVar right
      putStr ((show v) ++ "  ")
      putStrLn (show x) 
      listMode xs modulo right
      putMVar right (v+1)
    else do
      listMode xs modulo right

listMode1 :: [Int] -> Int -> MVar Int -> IO()
listMode1 [] _ _ = return ()
listMode1 l@(x:_) modulo right = listMode [x..((last l)-1)] modulo right 


listModeIORef :: [Int] -> Int -> IO()
listModeIORef [] _ = return ()
listModeIORef (x:xs) modulo =  if mtest x modulo 
    then do
      putStr "  "
      putStrLn (show x) 
      listModeIORef xs modulo
    else do
      listModeIORef xs modulo

listMode1IORef :: [Int] -> Int -> IO()
listMode1IORef [] _  = return ()
listMode1IORef l@(x:_) modulo = listModeIORef [x..((last l)-1)] modulo 
                      
