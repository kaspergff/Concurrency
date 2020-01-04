module Main where

import NetwerkFunctions
import Structure

import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import System.Environment
import System.IO
import Network.Socket
import Data.List

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
  -- initialization
  routingTabel    <- newTVarIO $ [Connection me 0 (-1)]
  messagecount    <- newTVarIO $ 0
  nbDistanceTable <- newTVarIO  []
   -- handle table
  htabel <- newTVarIO $ connection neighbours
  threadDelay 500
  -- make an instance of the node datatype which contains all info in this thread 
  let node = Node me routingTabel htabel nbDistanceTable messagecount 
  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections serverSocket lock' node
  -- -- Part 2 input
  _ <- forkIO $ inputHandler node lock'
    -- sendstatusmessage
  sendmystatusmessage node 
    -- send message MyDist
    -- this loop is used to make sure a node only sends out its mydistmessages when al its initial neighbours are online
  loop' messagecount node me neighbours
 
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
handleConnection connection' lock' n@(Node {handletable = h , neighbourDistanceTable = nt, nodeID = id',routingtable = rt,messageCount = mc}) = do
  chandle <- socketToHandle connection' ReadWriteMode
  line <- hGetLine chandle

  (messagetype,port,content)  <- atomically $ processline line 
  case messagetype of
    "Mystatus" ->  do
      atomically $ handlemystatus mc
    "Mydist" -> do

      (too,via,dis,oldDistance) <- atomically $ handlemydist content port n
      when (dis /= oldDistance) $ do 
        sendmydistmessage n too dis
        interlocked lock'$ putStrLn $ "Distance to " ++ show too ++ " is now " ++  show dis ++ " via " ++show via

    "ConnectRequest" -> do
      (intendedconnection) <- atomically $ handleconectrequest port h
      interlocked lock' $ putStrLn $ "Connected: " ++ show intendedconnection

    "StringMessage" -> do
      --port in this context means the intended destination
      let intendedreceiver = read port :: Int
      let message = content 
      if intendedreceiver == id'
        then interlocked lock' $ putStrLn (concat content)
        else do
          rt' <- atomically $ readTVar rt
          let bestneighbour = findbestneighbour intendedreceiver rt'
          handletable' <- atomically $ readTVar h
          interlocked lock' $ putStrLn $ "message for " ++ show intendedreceiver ++ " is relayed through " ++ show bestneighbour
          --find handle of best neighbour for the destination (port) 
          --send message to best neighbour for the destination d and send 'd' along to be used on the receiving side
          sendmessage (lookup bestneighbour handletable') ("StringMessage " ++ show intendedreceiver ++ " " ++ concat message)
    _ -> interlocked lock' $ putStrLn ("this message has no valid type and is therefore not sent to any neighbours" ++ line)
  hClose chandle

processline :: String -> STM (String,String,[String])
processline l = do
  let messagetype' = head (words l)
  let port'      = words l !! 1 
  let content'     = (words l \\ [messagetype']) \\ [port']
  return (messagetype',port',content')

handlemystatus :: TVar Int -> STM ()
handlemystatus mc = do
  mc' <- readTVar mc
  writeTVar mc (mc' + 1) 

handlemydist :: [String] -> String -> Node -> STM (Port,Port,Int,Int)
handlemydist content' port' n@(Node {routingtable = rt, neighbourDistanceTable = nt}) = do
  let v = read (head content') :: Int 
  let d = read (last content') :: Int
  let s = read port' :: Int
  updateNdisUTable nt (Connection s d v)
  rtable <- readTVar rt
  let oldDistance = getDistanceToPortFromRoutingTable rtable v
  (too, dis) <- recompute n v
  return (too,s,dis,oldDistance)

handleconectrequest :: String -> TVar HandleTable -> STM (Port)
handleconectrequest port h =  do
  let intendedconnection = read port :: Int
  let handle = intToHandle intendedconnection
  addToHandleTable h intendedconnection handle
  return (intendedconnection)
 

  -------------------- End Template---------------------

  --this function is used for looking up which node is the best neighbour when going to a third node
  --the -10000 indicates there is no best neighbour for the given port and the -10000 cannot be found in the handletable so no message will be sent
findbestneighbour :: Port -> Table -> Port
findbestneighbour _ [] = -10000
findbestneighbour distandneighbour ((Connection x _ y):xs) | distandneighbour == x =  y
                                                           | otherwise = findbestneighbour distandneighbour xs

updateNdisUTable :: TVar NeighbourDistanceTable -> Connection -> STM ()
updateNdisUTable nt con@(Connection from _ to ) = do
  table <- readTVar nt
  let newList = filter ( not.(\(Connection from' _ to') -> to' == to && from' == from)) table
  writeTVar nt $ newList ++ [con]
  return ()

createConnection :: Int -> Connection
createConnection int  = Connection int 1 int
     
initalRtable :: [Int] -> Table
initalRtable = map createConnection 

addToHandleTable :: (TVar HandleTable) -> Int -> IO Handle -> STM ()
addToHandleTable handletable neighbour handle = do
  htable <- readTVar handletable
  writeTVar handletable (htable ++ [(neighbour,handle)])

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
      printtabel <- atomically $ readTVar r
      interlocked lock' $ printRtable me printtabel
      inputHandler n lock'
    "B" -> do 
      routingtable' <- atomically $ readTVar r
      --find best neighbour for the destination (port) 
      let bestneighbour = findbestneighbour port routingtable'
      handletable' <- atomically $ readTVar h
      --find handle of best neighbour for the destination (port) 
      --send message to best neighbour for the destination d and send 'd' along to be used on the receiving side
      sendmessage (lookup bestneighbour handletable') ("StringMessage " ++ show port ++ " " ++ message)
      inputHandler n lock'
    "C" -> do 
      let handle = intToHandle port
      h' <- atomically $ readTVar h
      atomically $ writeTVar h (h' ++ [(port,handle)])
      sendmessage (Just handle) ("ConnectRequest " ++ show me)
      interlocked lock' $ putStrLn $ ("Connected: " ++ show port)
      inputHandler n lock'
    "D" -> do 
      printtabel <- atomically $ readTVar (neighbourDistanceTable n)
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

loop' :: TVar Int -> Node -> Int -> [Int] -> IO ()
loop' mc node me neighbours =  do
  messagecount' <- atomically $ readTVar mc
  if messagecount' /=  length neighbours
    then do
        threadDelay 2500000
        loop' mc node me neighbours
    else do
       sendmydistmessage node me 0
       threadDelay 1000000000
