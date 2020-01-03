module Main where

import NetwerkFunctions
import Structure

import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent.STM
--import Control.Concurrent.STM.TMVar
import Control.Exception
--import Data.IORef
import System.Environment
import System.IO
import Network.Socket
import Data.List
--import Data.Tuple


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  -- me :: Int is the port number of this process
  -- neighbours :: [Int] is a list of the port numbers of the initial neighbours
  -- During the execution, connections may be broken or constructed
  (me, neighbours) <- readCommandLineArguments
  putStrLn $ "I should be listening on port " ++ show me
  putStrLn $ "My initial neighbours are "     ++ show neighbours
  lock' <- newLock -- write lock
  -- Listen to the specified port.
  serverSocket <- socket AF_INET Stream 0
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ portToAddress me
  listen serverSocket 1024
  -- handle table
  htabel <- newTMVarIO $ connection neighbours
  -- initialization
  routingTabel    <- newTMVarIO $ Connection me 0 (-1) : [Connection a 999 (-2)| a <- neighbours]
  nbDistanceTable <- newTMVarIO  [Connection from 999 to | to <- neighbours, from <- neighbours]
  --nbDistanceTable <- newTMVarIO  []


  -- make an instance of the node datatype which contains all info in this thread 
  let node = Node me routingTabel htabel nbDistanceTable 

  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections serverSocket lock' node
  -- -- Part 2 input
  _ <- forkIO $ inputHandler node lock'
  threadDelay 8000
    -- send message MyDist
  sendmydistmessage node me 0
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
listenForConnections serverSocket lock' node = do
  (connection', _) <- accept serverSocket
  _ <- forkIO $ handleConnection connection' lock' node
  listenForConnections serverSocket lock' node

handleConnection :: Socket -> Lock -> Node -> IO ()
handleConnection connection' lock' n@(Node {handletable = h , neighbourDistanceTable = nt, nodeID = id',routingtable = rt}) = do
  --interlocked lock $ putStrLn "// Got new incomming connection"
  chandle <- socketToHandle connection' ReadWriteMode
  -- hPutStrLn chandle "// Welcome"
  line <- hGetLine chandle
  let messagetype = head (words line)
  let sender      = words line !! 1 
  let content     = (words line \\ [messagetype]) \\ [sender]
  
  
  case messagetype of
    --"Fail" -> do 
    "Mydist" -> do
      let v = read (head content) :: Int 
      let d = read (last content) :: Int
      let s = read sender :: Int
      atomically $ updateNdisUTable nt (Connection s d v)
      --interlocked lock' $putStrLn $ " 1e " ++ show s ++ " " ++ show d ++ " " ++ show v 
      rtable <- atomically $ readTMVar rt
      let oldDistance = getDistanceToPortFromRoutingTable rtable v
      (too, dis) <- atomically $ recompute n v
      when (dis /= oldDistance) $ sendmydistmessage n too dis  
    --"Repair" -> do
    "StringMessage" -> do
      --sender in this context means the intended destination
      --ik ga dit nog wel aanpassen maar doe maar ff alsof dit ok is
      let intendedreceiver = read sender :: Int
      let message = content 
      if intendedreceiver == id'
        then interlocked lock' $ putStrLn (concat content)
        else do
          rt' <- atomically $ readTMVar rt
          let bestneighbour = findbestneighbour intendedreceiver rt'
          handletable' <- atomically $ readTMVar h
          interlocked lock' $ putStrLn $ "message for " ++ show intendedreceiver ++ " is relayed through " ++ show bestneighbour
          --find handle of best neighbour for the destination (port) 
          --send message to best neighbour for the destination d and send 'd' along to be used on the receiving side
          sendmessage (lookup bestneighbour handletable') ("StringMessage " ++ show intendedreceiver ++ " " ++ concat message)
    _ -> interlocked lock' $ putStrLn ("this message has no valid type and is therefore not sent to any neighbours" ++ line)
  hClose chandle

  -------------------- End Template---------------------

  --this function is used for looking up which node is the best neighbour when going to a third node
  --note that this results in a maybe Int since some node may be removed the lookup process
findbestneighbour :: Port -> Table -> Port
findbestneighbour _ [] = -10000
findbestneighbour distandneighbour ((Connection x _ y):xs) | distandneighbour == x =  y
                                                           | otherwise = findbestneighbour distandneighbour xs

updateNdisUTable :: TMVar NeighbourDistanceTable -> Connection -> STM ()
updateNdisUTable nt con@(Connection from _ to ) = do
  table <- takeTMVar nt
  let newList = filterNot (\(Connection from' _ to') -> to' == to && from' == from) table
  putTMVar nt $ newList ++ [con]
  return ()

filterNot f = filter (not . f)

createConnection :: Int -> Connection
createConnection int  = Connection int 1 int
     
initalRtable :: [Int] -> Table
initalRtable = map createConnection 

-- addToHandleTable :: (TMVar HandleTable) -> Int -> IO Handle -> STM ()
-- addToHandleTable handletable neighbour handle = do
-- htable <- takeTMVar handletable
-- putTMVar handletable (htable ++ [(neighbour,handle)])

-- function to connect to all the neighbours 
connection :: [Port] -> HandleTable
connection xs = [(x, intToHandle x) | x <- xs]

intToHandle :: Port -> IO Handle
intToHandle i = do
  client <- connectSocket i
  chandle <- socketToHandle client ReadWriteMode
  return chandle

-- funtion to handle input
-- we moeten er op deze plaats voor zien de zorgen dat een functie word aangeroepen voor het printen van de tabel 
inputHandler :: Node -> Lock -> IO ()
inputHandler n@(Node {nodeID = me, routingtable = r, handletable = h}) lock' = do
  input <- getLine
  let (com, port, message) = inputParser input
  case com of
    "R" -> do 
      printtabel <- atomically $ readTMVar r
      interlocked lock' $ printRtable me printtabel
      inputHandler n lock'
    "B" -> do 
      routingtable' <- atomically $ readTMVar r
      --find best neighbour for the destination (port) 
      let bestneighbour = findbestneighbour port routingtable'
      handletable' <- atomically $ readTMVar h
      --find handle of best neighbour for the destination (port) 
      --send message to best neighbour for the destination d and send 'd' along to be used on the receiving side
      sendmessage (lookup bestneighbour handletable') ("StringMessage " ++ show port ++ " " ++ message)
      inputHandler n lock'
    "C" -> do 
      putStrLn $ "Command C"
      printtabel <- atomically $ readTMVar h
      mapM_ printHtable printtabel
      inputHandler n lock'
    "D" -> do 
      printtabel <- atomically $ readTMVar (neighbourDistanceTable n)
      interlocked lock' $ printRtable me printtabel
      inputHandler n lock'
    _ -> do
      putStrLn $ "wrong input"
      inputHandler n lock'

-- Needs improvement       
inputParser :: String -> (String, Int, String)
inputParser []    = ("", 0, "")-- the 0 is an placeholder
inputParser text  | length split < 2 = (com, 0, "") 
                  | length split < 3 = (com, port, "")
                  | otherwise        = (com, port, message)
  where
    split   = words text
    com     = head split
    port    = read (split !! 1) :: Int
    message = last split


-- funtion to print the routing table
printRtable :: Int -> Table -> IO ()
printRtable _ []     = return ()
printRtable _ table = mapM_ print table

-- function to print handle table
-- pls laat staan die kan nog nuttig zijn straks als ik een message moet routen
printHtable :: (Int,IO Handle) -> IO ()
printHtable (i, iOh) = do
  h <- iOh
  print $ show i ++ show h

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