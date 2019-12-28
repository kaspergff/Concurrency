module Main where

import NetwerkFunctions
import Structure

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket
import Data.List
import Data.Tuple


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- me :: Int is the port number of this process
  -- neighbours :: [Int] is a list of the port numbers of the initial neighbours
  -- During the execution, connections may be broken or constructed
  (me, neighbours) <- readCommandLineArguments

  putStrLn $ "I should be listening on port " ++ show me
  putStrLn $ "My initial neighbours are " ++ show neighbours

  lock <- newLock -- write lock

  -- Listen to the specified port.
  serverSocket <- socket AF_INET Stream 0
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ portToAddress me
  listen serverSocket 1024


  
  -- initialization
  routingTabel <- newTMVarIO $ [DConnection me 0 "local"] ++ [DConnection a 999 "udef"| a <- neighbours]
  nbDistanceTable <- newTMVarIO $ [Connection from 999 to| to <- neighbours, from <- neighbours]
  -- send message MyDist

  -- handle table
  htabel <- newTMVarIO $ connection neighbours


  -- make an instance of the node datatype which contains all info in this thread 
  let node = (Node me routingTabel htabel nbDistanceTable) 

  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections serverSocket lock node
  -- -- Part 2 input
  _ <- forkIO $ inputHandler node lock

  threadDelay 1000000000

readCommandLineArguments :: IO (Int, [Int])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, map read neighbours)

portToAddress :: Int -> SockAddr
portToAddress portNumber = SockAddrInet (fromIntegral portNumber) (tupleToHostAddress (127, 0, 0, 1)) -- localhost

connectSocket :: Int -> IO Socket
connectSocket portNumber = connect'
  where
    connect' = do
      client <- socket AF_INET Stream 0
      result <- try $ connect client $ portToAddress portNumber
      case result :: Either IOException () of
        Left _ -> do
          threadDelay 1000000
          connect'
        Right _ -> return client

listenForConnections :: Socket -> Lock -> Node -> IO ()
listenForConnections serverSocket lock node = do
  (connection, _) <- accept serverSocket
  _ <- forkIO $ handleConnection connection lock node
  listenForConnections serverSocket lock node

handleConnection :: Socket -> Lock -> Node -> IO ()
handleConnection connection lock n@(Node {handletable = h , neighbourDistanceTable = nt}) = do
  --interlocked lock $ putStrLn "// Got new incomming connection"
  chandle <- socketToHandle connection ReadWriteMode
  -- hPutStrLn chandle "// Welcome"
  line <- hGetLine chandle
  let messagetype = head (words line)
  let sender = read (words line !! 1) 
  let content = (((words line) \\ [messagetype]) \\ [sender])
  
  
  case (messagetype) of
    --("Fail") -> do 
    ("Mydist") -> do
      let v = read (head content) :: Int 
      let d = read (last content) :: Int
      let s = read (sender) :: Int
      atomically $ updateNdisUTable nt (Connection s d v) 
      atomically $ recompute n v  
    --("Repair") -> do
    ("StringMessage") -> do 
      interlocked lock $ putStrLn (concat content)
    (_) -> do
      interlocked lock $ putStrLn  "fakkadezewerktniet"
  hClose chandle

  -------------------- End Template---------------------

updateNdisUTable :: TMVar NeighbourDistanceTable -> Connection -> STM ()
updateNdisUTable nt con@(Connection from dis to ) = do
  table <- takeTMVar nt
  let newList = filter (\(Connection from' _ to') -> to' /= to && from' /= from) table
  putTMVar nt $ newList ++ [con]
  return ()

--hier moeten we dus nog voor zorgen dat er nog een distance berekend word en word meegegeven maar das voor later zorg
addtotable :: (TMVar Table) -> Int -> STM ()
addtotable routingtable neighbour = do 
  tabel <- takeTMVar routingtable
  putTMVar routingtable (tabel ++  [(Connection neighbour 1 neighbour)]) 

createConnection :: Int -> Connection
createConnection int  = Connection int 1 int
     
initalRtable :: [Int] -> Table
initalRtable xs = map createConnection xs


        
   

-- addToHandleTable :: (TMVar HandleTable) -> Int -> IO Handle -> STM ()
-- addToHandleTable handletable neighbour handle = do
-- htable <- takeTMVar handletable
-- putTMVar handletable (htable ++ [(neighbour,handle)])

sendmydistmessage :: Node -> Port -> Int ->  [IO ()]
sendmydistmessage n@(Node {nodeID = id, handletable = h}) to dist = do
  let handletable' = atomically $ readTMVar h
  map (flip sendmessage "jemoeder") (map snd handletable')



sendmessage :: Maybe (IO Handle) -> String -> IO ()
sendmessage (Just x) message = do
  x' <- x
  hSetBuffering x' LineBuffering
  hPutStrLn x' $ id message
sendmessage (Nothing) _ = putStrLn $ show  "error message"

-- function to connect to al the neighbours 
connection :: [Int] -> HandleTable
connection xs = [(x, intToHandle x)|x <- xs]

intToHandle :: Int -> IO Handle
intToHandle i = do
  client <- connectSocket i
  chandle <- socketToHandle client ReadWriteMode
  return chandle


-- funtion to handle input
-- we moeten er op deze plaats voor zien de zorgen dat een functie word aangeroepen voor het printen van de tabel 
inputHandler :: Node -> Lock -> IO ()
inputHandler n@(Node {nodeID = me, routingtable = r, handletable = h}) lock= do
  input <- getLine
  let (com, port, message) = inputParser input
  case (com) of
    ("R") -> do 
      printtabel <- atomically $ readTMVar r
      interlocked lock $ printRtable me printtabel
      inputHandler n lock
    ("B") -> do 
      handletable' <- atomically $ readTMVar h
      sendmessage ( lookup port handletable') ("StringMessage " ++ message)
      inputHandler n lock
    ("C") -> do 
      putStrLn $ "Command C"
      printtabel <- atomically $ readTMVar h
      mapM_ printHtable printtabel
      inputHandler n lock
    ("D") -> do 
      putStrLn $ "Command D"
      inputHandler n lock
    (_) -> do
      putStrLn $ "wrong input"
      inputHandler n lock

-- Needs improvement       
inputParser :: String -> (String, Int, String)
inputParser text  | text == []        = ("", 0, "")-- the 0 is an placeholder
                  | length split < 2  = (com, 0, "") 
                  | length split < 3  = (com, port, "")
                  | otherwise         = (com, port, message)
  where
    split = words text
    com = head split
    port = read (split !! 1) :: Int
    message = last split


-- funtion to print the routing table
printRtable :: Int -> Table -> IO ()
printRtable _ [] = return ()
printRtable me table = do
  mapM_ print table

printHtable :: (Int,IO Handle) -> IO ()
printHtable (i, iOHANDLE) = do
  handle <- iOHANDLE
  print $ show i ++ show handle

newtype Lock = Lock (MVar ())

newLock :: IO Lock
newLock = Lock <$> newMVar ()

lock :: Lock -> IO ()
lock (Lock v) = takeMVar v

unlock :: Lock -> IO ()
unlock (Lock v) = putMVar v ()

interlocked :: Lock -> IO a -> IO a
interlocked lock_ action =
    (lock lock_ *> action) `finally` (unlock lock_)