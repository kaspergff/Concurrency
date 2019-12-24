
module Main where

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

--datatypes
--vanaf nu is een node gwn lekker een node
data Node = Node {
  nodeID       :: Int,
  routingtable :: (TMVar Table),
  handletable  :: (TMVar HandleTable)
  }  
--we moeten die tabel gaan zien als een reachability graph
--vanaf nu zijn de connecties gwn lekker een eigen type
data Connection = Connection Int Int Int
instance Show Connection where
  show (Connection a b c) = show a ++ " "++ show b ++ " " ++ show c
--tabel is een lijst van connecties
type Table = [Connection] 
type NodeHandle = (Int,IO Handle)
type HandleTable = [NodeHandle]

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

  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections serverSocket lock
  -- routing table
  tabel <- newTMVarIO $ initalRtable neighbours
  -- handle table
  htabel <- newTMVarIO $ connection neighbours


  -- make an instance of the node datatype which contains all info in this thread 
  let node = (Node me tabel htabel) 
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

listenForConnections :: Socket -> Lock-> IO ()
listenForConnections serverSocket lock = do
  (connection, _) <- accept serverSocket
  _ <- forkIO $ handleConnection connection lock
  listenForConnections serverSocket lock

handleConnection :: Socket -> Lock -> IO ()
handleConnection connection  lock= do
  --interlocked lock $ putStrLn "// Got new incomming connection"
  chandle <- socketToHandle connection ReadWriteMode
  -- hPutStrLn chandle "// Welcome"
  message <- hGetLine chandle
  interlocked lock $ putStrLn message
  hClose chandle

  -------------------- End Template---------------------

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
--   htable <- takeTMVar handletable
--   putTMVar handletable (htable ++ [(neighbour,handle)])


-- function to write a messsage from node a to b (kunnen we straks mooi gebruiken voor een astractie van de fail en repair messaged ed)
-- string is voor nu het datatype maar kan mis beter een tuple worden van typebericht en bericht of we kunnen een datatype bericht maken dat
-- Fail| repair | message is
-- hij schrijft wel een bericht maar het kan maar zo dat hij constant bezig is met het toevoegen van nieuwe connecties tussen nodes die al geconnect zijn
-- het gekke is dus dat hij eig een nieuwe connectie maakt maar je hebt die handle wel nodig om dat bericht te sturen
sendmessage :: Maybe (IO Handle) -> String -> IO ()
sendmessage (Just x) message = do
  x' <- x
  hSetBuffering x' LineBuffering
  hPutStrLn x' $ id message
sendmessage (Nothing) _ = putStrLn $ "error message"

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
      printRtable me printtabel
      inputHandler n lock
    ("B") -> do 
      handletable' <- atomically $ readTMVar h
      sendmessage ( lookup port handletable') message
      inputHandler n lock
    ("C") -> do 
      putStrLn $ "Command C"
      -- printtabel <- atomically $ readTMVar h
      -- mapM_ printHtable printtabel
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
  putStrLn $ show me ++ " 0 local"
  mapM_ print table

printHtable :: (Int,IO Handle) -> IO ()
printHtable (i, iOHANDLE) = do
  handle <- iOHANDLE
  print $ show i ++ show handle

-- foo :: (IO a) -> a
-- foo x = do
--   x' <- x
--   return (x')




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