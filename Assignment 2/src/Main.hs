module Main where

import NetwerkFunctions
import Structure

import Control.Monad (when, forM_)
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

--this function is used for performing different actions for different types of incomming messages
handleConnection :: Socket -> Lock -> Node -> IO ()
handleConnection connection' lock' n@(Node {handletable = h , neighbourDistanceTable = nt, nodeID = id',routingtable = rt,messageCount = mc}) = do
  chandle <- socketToHandle connection' ReadWriteMode
  line <- hGetLine chandle

  (messagetype,port,content)  <- atomically $ processline line 
  case messagetype of
    --if a mystatus message is received one is added to a counter which is used to determine if all neighbours are active
    "Mystatus" ->  do
      atomically $ handlemystatus mc
    "Mydist" -> do
      (too,via,dis,oldDistance) <- atomically $ handlemydist content port n
      when (dis /= oldDistance && dis <= 24) $ do 
        sendmydistmessage n too dis
        interlocked lock'$ putStrLn $ "Distance to " ++ show too ++ " is now " ++  show dis ++ " via " ++show via
      when (dis /= oldDistance && dis > 23) $  interlocked lock'$ putStrLn $ "Unreachable: "++ show too
    --if a connectrequest message is received the node adds the sending node and its handle to the handletable for future communications
    "ConnectRequest" -> do
      (intendedconnection) <- atomically $ handleconectrequest port h nt
      interlocked lock' $ putStrLn $ "Connected: " ++ show intendedconnection
      repair n intendedconnection lock'
    --if a stringmessage is received the process checks if is has to be send to the next neighbour for a given destination or if it is intended for the node in question
    --please note that even though the routing table may not always be correct the message always gets to the desired destination by following the foulty routing table
    "StringMessage" -> do
      --port in this context means the intended destination
      let intendedreceiver = read port :: Int
      let message = content 
      if intendedreceiver == id'
        then interlocked lock' $ putStrLn (concat content)
        else sendToNextNode lock' h rt message intendedreceiver
    "DisConnectRequest" -> do
      --remove from handlelist
      let intendedreceiver = read port :: Int
      handletable' <- atomically $ readTVar h
      let handle = (lookup intendedreceiver handletable')
      atomically $ removeFromHandleTable h intendedreceiver
      --close channel
      --perform recompute
      
      interlocked lock' $ putStrLn $ "Disconnected" ++ show intendedreceiver
    _ -> interlocked lock' $ putStrLn ("this message has no valid type and is therefore not sent to any neighbours, also no action is taken" ++ line)
  hClose chandle

sendToNextNode :: Lock -> TVar HandleTable -> TVar Table -> [String] -> Int -> IO ()
sendToNextNode lock' h rt message intendedreceiver = do
  rt' <- atomically $ readTVar rt
  let bestneighbour = findbestneighbour intendedreceiver rt'
  handletable' <- atomically $ readTVar h
  interlocked lock' $ putStrLn $ "message for " ++ show intendedreceiver ++ " is relayed through " ++ show bestneighbour
  --find handle of best neighbour for the destination (port) 
  --send message to best neighbour for the destination d and send 'd' along to be used on the receiving side
  sendmessage (lookup bestneighbour handletable') ("StringMessage " ++ show intendedreceiver ++ " " ++ concat message)


--note that the functions processline,handlemystatus,handlemydist and handleconectrequest are used by handleconnection. they are all made in the STM type so they can be executed actomically in the handleconnection function

--this function is used to disect the incomming messages 
processline :: String -> STM (String,String,[String])
processline l = do
  let messagetype' = head (words l)
  let port'        = words l !! 1 
  let content'     = (words l \\ [messagetype']) \\ [port']
  return (messagetype',port',content')

--this function is used to add one to the active neighbour counter
handlemystatus :: TVar Int -> STM ()
handlemystatus mc = do
  mc' <- readTVar mc
  writeTVar mc (mc' + 1) 

--this function is used to activate recompute upon receiving a mydist message
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

handleconectrequest :: String -> TVar HandleTable -> TVar NeighbourDistanceTable -> STM (Port)
handleconectrequest port h ndt=  do
  let intendedconnection = read port :: Int
  let handle = intToHandle intendedconnection
  addToHandleTable h intendedconnection handle
  -- add port to ndis
  updateNdisUTable ndt (Connection intendedconnection 0 intendedconnection)
  return (intendedconnection)
 

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
  writeTVar nt $ [con] ++ newList
  return ()

--function for adding a single entry of the (Int, IO Handle) type to the handle table
addToHandleTable :: (TVar HandleTable) -> Int -> IO Handle -> STM ()
addToHandleTable handletable neighbour handle = do
  htable <- readTVar handletable
  writeTVar handletable (htable ++ [(neighbour,handle)])

removeFromHandleTable :: (TVar HandleTable) -> Int -> STM()
removeFromHandleTable handletable neighbour = do
  htable <- readTVar handletable
  let newlist = filter (not.(\(x,_) -> x == neighbour)) htable
  writeTVar handletable newlist


-- function to connect to all the neighbours 
connection :: [Port] -> HandleTable
connection = zip <*> map intToHandle

intToHandle :: Port -> IO Handle
intToHandle i = do
  client <- connectSocket i
  chandle <- socketToHandle client ReadWriteMode
  return chandle

-- funtion to handle input
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
      atomically $ addToHandleTable h port handle 
      sendmessage (Just handle) ("ConnectRequest " ++ show me)
      interlocked lock' $ putStrLn $ ("Connected: " ++ show port)
      repair n port lock'

    "D" -> do
      handletable' <- atomically $ readTVar h
      let handle = (lookup port handletable')
      sendmessage handle ("DisConnectRequest " ++ show me)
      interlocked lock' $ putStrLn $ ("Disconnected: " ++ show port)
      fail' n port lock' handle
    --                                             --
    --                                             --
    -- E is debug dus verwijder voor de eindversie --
    --                                             --
    --                                             --
    "E" -> do 
      printtabel <- atomically $ readTVar (neighbourDistanceTable n)
      interlocked lock' $ printRtable me printtabel
      inputHandler n lock'
    _ -> do
      putStrLn $ "wrong input"
      inputHandler n lock'

-- this parser is used to parse the commands given in the console       
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

---                                                --
--- DEZE IS OOK DEBUG EN MOET WEG IN DE EINDVERSIE --
---                                                --
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
        threadDelay 2000000
        loop' mc node me neighbours
    else do
       sendmydistmessage node me 0
       threadDelay 1000000
       loop' mc node me neighbours

repair :: Node -> Port -> Lock -> IO()
repair n port lock= do
    -- add new neigbour to routingtable
    atomically $ addToRoutingTable (routingtable n) (Connection port 1 port)
    routingtable' <- atomically $ readTVar (routingtable n)

    -- denk dat t t beste is als dit interlocked gebeurt
    interlocked lock $ do
      forM_ routingtable' $ \(Connection too dis _) -> do

          -- the code below that is comment out is to implement the line  ndisu[w, v] := N ; But if its is used tomjudge turns red
          -- ndisu <- atomically $ readTVar (neighbourDistanceTable n)
          -- --let updateNdis = map (\con@(Connection f d t)-> if t == too then updateDistance con 24 else con) ndisu
          -- atomically $ writeTVar (neighbourDistanceTable n) (map (\con@(Connection f d t)-> 
          --   if t == too && getDistanceToPortFromRoutingTable routingtable' f == 1
          --     then updateDistance con 24 
          --   else con) ndisu)
          sendmydistmessage n too dis

fail' :: Node -> Port -> Lock -> Maybe (IO Handle)-> IO()
fail' n@(Node {handletable = h}) port lock' handle = do
  atomically $ removeFromHandleTable h port 
  --doe algoritme dingen
  --doe hclose met doosje handle
 


updateDistance :: Connection -> Int -> Connection
updateDistance (Connection a _ b) newdis = Connection a newdis b
