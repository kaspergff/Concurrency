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
      SyncMVar -> mVarCount (cfgThreads config) ints (cfgModulus config)
      SyncIORef -> iORefCount (cfgThreads config) ints (cfgModulus config)
    List  -> case cfgSync config of
      SyncMVar -> mVarList (cfgThreads config) ints (cfgModulus config)
      SyncIORef -> iORefList (cfgThreads config) ints (cfgModulus config)
    Search str -> case cfgSync config of
      SyncMVar -> mVarSearch (cfgThreads config) ints (cfgModulus config) str 
      SyncIORef -> putStrLn "fakkadeezisnognietgeimplementeerd" 
    _ -> return ()
    

  threadDelay 10000

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



--
--
--
--General functions that are both used by Ioref and Mvar
--
--
--split list, get the first Nth part of the list  
getListPart :: Int -> [Int] -> [Int]
getListPart 1 list = list
getListPart n list = take (floor (1 / (fromIntegral n) * (fromIntegral $ length list))) list
 
--Mtest functions
digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

weights :: Int -> [Int]
weights n = reverse [1..(length (digits n))]

mtest :: Int -> Int -> Bool
mtest number m = mod (sum(zipWith (*) (digits number) (weights number))) m == 0

--mtest for every 1st thread in count mode
countMode1 :: [Int] -> Int -> Int
countMode1 [] _= 0
countMode1 l@(x:_) modulo = countMode [x..((last l)-1)] modulo  

--mtest for every Nth thread in countmode
countMode :: [Int] -> Int -> Int
countMode [] _ = 0
countMode list modulo = length [x | x <- list, mtest x modulo]



--
--
--
-- Ioref functions
--
--
-- Locks
-- Guard some action using the given lock
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

--countmode
iORefCount :: Int -> [Int] -> Int -> IO ()
iORefCount threads list modulo = do
  lock <- newIORef Unlocked
  counter <- newIORef 0 
  iORefmakeForkCount counter lock threads list modulo
  threadDelay 1000
  c <- readIORef counter
  putStrLn (show c)

iORefmakeForkCount :: IORef Int -> IORef Lock -> Int -> [Int] -> Int -> IO ()
iORefmakeForkCount _  _ 0 _ _ = return ()
iORefmakeForkCount c lock 1 ints modulo  = do
  _ <- forkIO $ do
    let count = countMode1 ints modulo
    interlocked lock (iORefCountAction c count)
  return ()
iORefmakeForkCount c lock n ints modulo = do
  _ <- forkIO $ do
    let count = countMode (getListPart n ints) modulo
    interlocked lock (iORefCountAction c count)
  iORefmakeForkCount c lock (n-1) (ints \\ (getListPart n ints)) modulo

iORefCountAction :: Num a => IORef a -> a -> IO ()
iORefCountAction c count = do 
  old <- readIORef c
  writeIORef c (old + count) 

--listmode
iORefList :: Int -> [Int] -> Int -> IO ()
iORefList threads list modulo = do
  lock <- newIORef Unlocked
  counter <- newIORef 0
  threadDelay 10000
  iORefListFork threads list modulo lock counter

iORefListFork :: Int -> [Int] -> Int -> IORef Lock -> IORef Int-> IO ()
iORefListFork 0 _ _ _ _ = return ()
iORefListFork 1 ints modulo lock counter = do
  _ <- forkIO $ do 
    listMode1IORef ints modulo lock counter
  return ()
iORefListFork n ints modulo lock counter = do
  _ <- forkIO $ do
    listModeIORef (getListPart n ints) modulo lock counter
  iORefListFork (n-1) (ints \\ (getListPart n ints)) modulo lock counter

listModeIORef :: [Int] -> Int -> IORef Lock -> IORef Int-> IO()
listModeIORef [] _ _ _ = return ()
listModeIORef (x:xs) modulo lock counter = if mtest x modulo 
    then do
      interlocked lock (writeActionListIORef x counter)
      listModeIORef xs modulo lock counter
    else do
      listModeIORef xs modulo lock counter
  
writeActionListIORef :: Int -> IORef Int-> IO ()
writeActionListIORef x counter = do
  oldCounter <- readIORef counter
  let newCounter = oldCounter + 1
  writeIORef counter newCounter
  putStrLn $ (show newCounter) ++ " " ++ (show x)
  
listMode1IORef :: [Int] -> Int -> IORef Lock-> IORef Int -> IO()
listMode1IORef [] _ _ _ = return ()
listMode1IORef l@(x:_) modulo lock counter = listModeIORef [x..((last l)-1)] modulo lock counter

pollingI :: IORef Int -> Int -> String ->IO ()  
pollingI counter threads s = do
  c <- readIORef counter
  if (c == threads && c > -1) 
    then do
      putStr s
    else if c > -1 
      then do 
      putStrLn $ show c
      threadDelay 100
      pollingI counter threads s
      else 
        return()


--
--
-- Mvar functions
--
--
--

--countmode
mVarCount :: Int -> [Int] -> Int -> IO ()
mVarCount threads list modulo = do
  counter <- newMVar 0 
  nmrofthreads <- newMVar 0
  mVarmakeForkCount counter nmrofthreads threads list modulo
  polling nmrofthreads threads counter
 
  

mVarmakeForkCount :: MVar Int -> MVar Int -> Int -> [Int] -> Int -> IO ()
mVarmakeForkCount _ _ 0 _ _ = return ()
mVarmakeForkCount c nmrofthreads 1 ints modulo  = do
  _ <- forkIO $ do 
    let count = countMode1 ints modulo
    old <- takeMVar c
    putMVar c (old + count)
    nmr <- takeMVar nmrofthreads
    putMVar nmrofthreads (nmr+1)
  return ()
mVarmakeForkCount c nmrofthreads n ints modulo  = do
  _ <- forkIO $ do
    let count = countMode (getListPart n ints) modulo
    old <- takeMVar c
    putMVar c (old + count)
    nmr <- takeMVar nmrofthreads
    putMVar nmrofthreads (nmr+1)
  mVarmakeForkCount c nmrofthreads (n-1) (ints \\ (getListPart n ints)) modulo

--listmode
mVarList :: Int -> [Int] -> Int -> IO ()
mVarList threads list modulo = do
  writelock <- newMVar 1
  actievethreads <- newMVar 0 
  mVarListFork threads list modulo writelock actievethreads
  polling1 actievethreads threads

mVarListFork :: Int -> [Int] -> Int -> MVar Int -> MVar Int -> IO ()
mVarListFork 0 _ _ _ _ = return ()
mVarListFork 1 ints modulo right nmrofthreads = do
  _ <- forkIO $ do 
    listModeMVar1 ints modulo right
    nmr <- takeMVar nmrofthreads
    putMVar nmrofthreads (nmr+1)
  return ()
mVarListFork n ints modulo right nmrofthreads = do
  _ <- forkIO $ do
    listModeMVar (getListPart n ints) modulo right
    nmr <- takeMVar nmrofthreads
    putMVar nmrofthreads (nmr+1)
  mVarListFork (n-1) (ints \\ (getListPart n ints)) modulo right nmrofthreads
 

listModeMVar :: [Int] -> Int -> MVar Int -> IO()
listModeMVar [] _ _ = return ()
listModeMVar (x:xs) modulo right =  if mtest x modulo 
    then do     
      v <- takeMVar right
      putStr ((show v) ++ " ")
      putStrLn (show x) 
      putMVar right (v+1)
      listModeMVar xs modulo right
    else do
      listModeMVar xs modulo right

listModeMVar1 :: [Int] -> Int -> MVar Int -> IO()
listModeMVar1 [] _ _ = return ()
listModeMVar1 l@(x:_) modulo right = listModeMVar [x..((last l)-1)] modulo right 

--searchmode
--the functions below might look a bit odd but that is due to the fact that an async thread has a certain procedure in order to be cancelled (async cannot be killed when it enters a wait functions)
mVarSearch :: Int -> [Int] -> Int -> ByteString -> IO ()
mVarSearch threads list modulo str  = do
  threadList <- newMVar [] -- list to threads to be killed if number is found
  extraList <- newMVar [] -- list of threads that is finished
  mVarSearchFork threads list modulo threadList extraList str 
  a <- readMVar extraList--this is looked at when all threads are complete
  if (length a) == threads then -- if the length of the list that contains all the finished treads is the same as the amount of started threads than the number is not found so.....that is printed
    putStrLn "not found"
  else return ()
 

mVarSearchFork :: Int -> [Int] -> Int -> MVar [Async ()]-> MVar [Async ()]-> ByteString -> IO ()
mVarSearchFork 0 _ _ _ _ _ =  return ()
mVarSearchFork 1 ints modulo right tlist str  = makeMvarSearchFork searchMode1 ints 1 ints modulo right tlist str
mVarSearchFork n ints modulo right tlist str  = makeMvarSearchFork searchMode (getListPart n ints) n ints modulo right tlist str
    
--makeMvarSearchFork :: (a->b) -> [Int] -> Int -> [Int] -> Int -> MVar [Async ()]-> MVar [Async ()]-> ByteString -> IO ()
makeMvarSearchFork f1 f2 n ints modulo right tlist str = do
  aid <- async $ do
    f1 f2 modulo right tlist str 
  list <- takeMVar right
  putMVar right (list ++ [aid]) 
  list3 <- takeMVar tlist
  putMVar tlist (list3 ++ [ aid]) 
  mVarSearchFork (n-1) (ints \\ (getListPart n ints)) modulo right tlist str 
  list1 <- readMVar right
  when (aid `elem` list1) ( do
    list2 <-takeMVar right
    putMVar right (list2 \\ [aid])
    --before the wait an async does not like to be killed, it threw an error because of it so this way it is errorless.
    wait aid
    return ())

searchMode :: [Int] -> Int -> MVar [Async ()]-> MVar [Async ()] -> ByteString -> IO()
-- [] means not found is this thread
searchMode [] _ _ _ _  = return ()
searchMode (x:xs) modulo right tlist str =  if mtest x modulo && checkHash str x
    then do
      ids <- takeMVar right --get all threads that need to be killed
      _ <- takeMVar tlist -- make sure that "not found" cannot be printed
      putStrLn (show x) -- show the found number
      putMVar tlist [] -- make sure that "not found" can never be printed beyond this point
      putMVar right [] -- empty the active processes list
      mapM_ cancel  (ids) -- cancel the processes that where active at the time the number was found
      return ()
      else do
      -- if the answer was not in the list so far, continue to search through the list.
      searchMode xs modulo right tlist str

searchMode1 :: [Int] -> Int -> MVar [Async ()]-> MVar [Async ()]-> ByteString -> IO()
searchMode1 [] _ _ _ _  = return ()
searchMode1 l@(x:_) modulo right tlist str  = searchMode [x..((last l)-1)] modulo right tlist str                 

polling :: MVar Int -> Int -> MVar Int ->IO ()  
polling counter threads s = do
  c <- readMVar counter
  a <- readMVar s
  if (c == threads && c > -1) 
    then do
      putStr (show a)
    else if (c > -1) 
      then do 
      threadDelay 1000
      polling counter threads s
      else 
        return()

polling1 :: MVar Int -> Int ->IO ()  
polling1 counter threads = do
  c <- readMVar counter
  if (c == threads && c > -1) 
    then do
      return ()
    else if (c > -1) 
      then do 
      threadDelay 1000
      polling1 counter threads 
      else 
        return()
