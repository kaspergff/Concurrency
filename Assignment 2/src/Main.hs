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
  forkIO $ listenForConnections serverSocket lock' node
  -- -- Part 2 input
  forkIO $ inputHandler node lock'
    -- sendstatusmessage
  sendMyStatusMessage node 
    -- send message MyDist
    -- this loop is used to make sure a node only sends out its mydistmessages when al its initial neighbours are online
  loop' messagecount node me neighbours
 


  threadDelay 1000000000000000000000000000



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

  (messagetype,port,content)  <- atomically $ processLine line 
  case messagetype of
    --if a mystatus message is received one is added to a counter which is used to determine if all neighbours are active
    "Mystatus" ->  do
      atomically $ handleActivationMessage mc
    -- message that distance from nb to different node has changed
    "Mydist" -> do
      (too,via,dis,oldDistance) <- atomically $ handleMyDistMessage content port n
      when (dis /= oldDistance && dis < 24) $ do 
        sendMyDistMessage n too dis
        interlocked lock'$ putStrLn $ "Distance to " ++ show too ++ " is now " ++  show dis ++ " via " ++show via
      when (dis > 23) $ do
         interlocked lock'$ putStrLn $ "Unreachable: "++ show too
    --if a connectrequest message is received the node adds the sending node and its handle to the handletable for future communications
    "ConnectRequest" -> do
      (intendedconnection) <- atomically $ handleConnectRequest port h nt
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
      fail' n intendedreceiver lock' handle
      --close channel
      --perform recompute
      
      interlocked lock' $ putStrLn $ "Disconnected " ++ show intendedreceiver
    _ -> interlocked lock' $ putStrLn ("this message has no valid type and is therefore not sent to any neighbours, also no action is taken" ++ line)
  hClose chandle

sendToNextNode :: Lock -> TVar HandleTable -> TVar Table -> [String] -> Int -> IO ()
sendToNextNode lock' h rt message intendedreceiver = do
  rt' <- atomically $ readTVar rt
  let bestneighbour = findBestNeighbour intendedreceiver rt'
  handletable' <- atomically $ readTVar h
  interlocked lock' $ putStrLn $ "message for " ++ show intendedreceiver ++ " is relayed through " ++ show bestneighbour
  --find handle of best neighbour for the destination (port) 
  --send message to best neighbour for the destination d and send 'd' along to be used on the receiving side
  sendMessage (lookup bestneighbour handletable') ("StringMessage " ++ show intendedreceiver ++ " " ++ concat message)


--note that the functions processLine,handleActivationMessage,handleMyDistMessage and handleConnectRequest are used by handleconnection. they are all made in the STM type so they can be executed actomically in the handleconnection function

--this function is used to disect the incomming messages 
processLine :: String -> STM (String,String,[String])
processLine l = do
  let messagetype' = head (words l)
  let port'        = words l !! 1 
  let content'     = (words l \\ [messagetype']) \\ [port']
  return (messagetype',port',content')

--this function is used to add one to the active neighbour counter
handleActivationMessage :: TVar Int -> STM ()
handleActivationMessage mc = do
  mc' <- readTVar mc
  writeTVar mc (mc' + 1) 

--this function is used to activate recompute upon receiving a mydist message
handleMyDistMessage :: [String] -> String -> Node -> STM (Port,Port,Int,Int)
handleMyDistMessage content' port' n@(Node {routingtable = rt, neighbourDistanceTable = nt}) = do
  let v = read (head content') :: Int 
  let d = read (last content') :: Int
  let s = read port' :: Int
  updateNdisUTable nt (Connection s d v)
  rtable <- readTVar rt
  let oldDistance = getDistanceToPortFromRoutingTable rtable v
  (too, dis, via) <- recompute n v
  return (too,read via,dis,oldDistance)

handleConnectRequest :: String -> TVar HandleTable -> TVar NeighbourDistanceTable -> STM (Port)
handleConnectRequest port h ndt=  do
  let intendedconnection = read port :: Int
  let handle = intToHandle intendedconnection
  addToHandleTable h intendedconnection handle
  -- add port to ndis
  updateNdisUTable ndt (Connection intendedconnection 0 intendedconnection)
  return (intendedconnection)
 

  --this function is used for looking up which node is the best neighbour when going to a third node
  --the -10000 indicates there is no best neighbour for the given port and the -10000 cannot be found in the handletable so no message will be sent
findBestNeighbour :: Port -> Table -> Port
findBestNeighbour _ [] = -10000
findBestNeighbour distandneighbour ((Connection x _ y):xs) | distandneighbour == x =  y
                                                           | otherwise = findBestNeighbour distandneighbour xs

-- function to update the ndisu table
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

-- function to remove a handle from the handle table
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
      let bestneighbour = findBestNeighbour port routingtable'
      handletable' <- atomically $ readTVar h
      --find handle of best neighbour for the destination (port) 
      --send message to best neighbour for the destination d and send 'd' along to be used on the receiving side
      sendMessage (lookup bestneighbour handletable') ("StringMessage " ++ show port ++ " " ++ message)
      inputHandler n lock'
    "C" -> do 
      let handle = intToHandle port
      atomically $ addToHandleTable h port handle 
      sendMessage (Just handle) ("ConnectRequest " ++ show me)
      interlocked lock' $ putStrLn $ ("Connected: " ++ show port)
      repair n port lock'
      inputHandler n lock'

    "D" -> do
      handletable' <- atomically $ readTVar h
      if any (==port) (map fst handletable') && port /= me
        then do
            let handle = (lookup port handletable')
            sendMessage handle ("DisConnectRequest " ++ show me)
            interlocked lock' $ putStrLn $ ("Disconnected: " ++ show port)
            fail' n port lock' handle
            inputHandler n lock'
      else do
            interlocked lock' $ putStrLn $ ("these nodes where not even connected in the first place")
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

-- this loop is made to make shure al our nb are ready to start 
loop' :: TVar Int -> Node -> Int -> [Int] -> IO ()
loop' mc node me neighbours =  do
  messagecount' <- atomically $ readTVar mc
  if messagecount' <  length neighbours
    then do
        threadDelay 2000000
        loop' mc node me neighbours
    else do
       sendMyDistMessage node me 0
       threadDelay 100000
       loop' mc node me neighbours


-- function to implement the repair mode of the algortihm
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
          sendMyDistMessage n too dis


-- fail functoin of the algorithm. Used in disconnect
fail' :: Node -> Port -> Lock -> Maybe (IO Handle)-> IO()
fail' _ _ _ (Nothing) = return ()
fail' n@(Node {handletable = h}) port lock' (Just handle) = do
  interlocked lock' $ do
    -- remove handle
    atomically $ removeFromHandleTable h port

    -- make the node unreachable
    rt <- atomically $ readTVar (routingtable n)
    atomically $ writeTVar (routingtable n) (map (\con@(Connection from _ to)->
      if from == port && to == port
        then updateDistance con 24 
      else con) rt) 

    -- remove node from ndisu table
    ndisu <- atomically $ readTVar (neighbourDistanceTable n)
    let filterNdisu = filter (\(Connection p _ _) -> p /= port) ndisu 
    atomically $ writeTVar (neighbourDistanceTable n) filterNdisu
    -- start recompute for all nodes 
    routingtable' <- atomically $ readTVar (routingtable n)

    -- do the recompute part
    sendMyDistMessage n port 24
    forM_ routingtable' $ \(Connection too _dis _) -> do
        let oldDistance = getDistanceToPortFromRoutingTable routingtable' too
        (too, dis, via) <- atomically $ recompute n too
        when (dis /= oldDistance && dis < 24) $ do 
          sendMyDistMessage n too dis
          putStrLn $ "Distance to " ++ show too ++ " is now " ++  show dis ++ " via " ++show via
        when (dis > 23) $  putStrLn $ "Unreachable: "++ show too

-- update distance used in repair.fail -> improve
updateDistance :: Connection -> Int -> Connection
updateDistance (Connection a _ _) newdis = Connection a newdis (-2)

-- write lock
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
