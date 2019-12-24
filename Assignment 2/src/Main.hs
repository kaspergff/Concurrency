
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

--datatypes
--vanaf nu is een node gwn lekker een node
data Node = Node {
  nodeID :: Int,
  routingtable :: (TMVar Table),
  handletable :: (TMVar HandleTable)
  }  
--we moeten die tabel gaan zien als een reachability graph
--vanaf nu zijn de connecties gwn lekker een eigen type
data Connection = Connection Int Int Int deriving (Show)
--tabel is een lijst van connecties
type Table = [Connection] 
type NodeHandle = (Int,Handle)
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

  -- Listen to the specified port.
  serverSocket <- socket AF_INET Stream 0
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ portToAddress me
  listen serverSocket 1024

  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections serverSocket
  -- routing table
  tabel <- newTMVarIO []
  -- handle table
  htabel <- newTMVarIO []
  -- make an instance of the node datatype which contains all info in this thread 
  let node = (Node me tabel htabel) 
  -- Part 1 Initialisation (Geen idee of dit persee in een apparte thread moet)
  _ <- forkIO $ initialisation node neighbours 
  -- -- Part 2 input
  _ <- forkIO $ inputHandler node

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

listenForConnections :: Socket -> IO ()
listenForConnections serverSocket = do
  (connection, _) <- accept serverSocket
  _ <- forkIO $ handleConnection connection
  listenForConnections serverSocket

handleConnection :: Socket -> IO ()
handleConnection connection = do
  putStrLn "Got new incomming connection"
  chandle <- socketToHandle connection ReadWriteMode
  hPutStrLn chandle "Welcome"
  message <- hGetLine chandle
  putStrLn $ "Incomming connection send a message: " ++ message
  hClose chandle


  -------------------- End Template---------------------
-- This function sets up the network en tries to connect to al the neighbours
initialisation :: Node -> [Int] -> IO ()
initialisation _ []             = do putStrLn "I have no more neighbours :("
initialisation me (neighbour:xs)  = do
  makeConnnection me neighbour 
  initialisation me xs 

--hier moeten we dus nog voor zorgen dat er nog een distance berekend word en word meegegeven maar das voor later zorg
addtotable :: (TMVar Table) -> Int -> STM ()
addtotable routingtable neighbour = do 
  tabel <- takeTMVar routingtable
  putTMVar routingtable (tabel ++  [(Connection neighbour 1 neighbour)]) 

addToHandleTable :: (TMVar HandleTable) -> Int -> Handle -> STM ()
addToHandleTable handletable neighbour handle = do
  htable <- takeTMVar handletable
  putTMVar handletable (htable ++ [(neighbour,handle)])


-- function to write a messsage from node a to b (kunnen we straks mooi gebruiken voor een astractie van de fail en repair messaged ed)
-- string is voor nu het datatype maar kan mis beter een tuple worden van typebericht en bericht of we kunnen een datatype bericht maken dat
-- Fail| repair | message is
-- hij schrijft wel een bericht maar het kan maar zo dat hij constant bezig is met het toevoegen van nieuwe connecties tussen nodes die al geconnect zijn
-- het gekke is dus dat hij eig een nieuwe connectie maakt maar je hebt die handle wel nodig om dat bericht te sturen
sendmessage :: Int -> Int -> String -> IO ()
sendmessage from to message = do 
  client <- connectSocket to
  chandle <- socketToHandle client ReadWriteMode
  hPutStrLn chandle $ "Hi process " ++ show to ++ "! I'm process " ++ show from ++ " and i wanted to say" ++ show message

-- function to make a connection between two nodes  
makeConnnection :: Node -> Int -> IO ()
makeConnnection n@(Node {nodeID = me ,routingtable = r, handletable = h}) neighbour = do 
  putStrLn $ "Connecting to neighbour " ++ show neighbour ++ "..."
  client <- connectSocket neighbour
  chandle <- socketToHandle client ReadWriteMode
  -- Send a message over the socket
  -- You can send and receive messages with a similar API as reading and writing to the console.
  -- Use `hPutStrLn chandle` instead of `putStrLn`,
  -- and `hGetLine  chandle` instead of `getLine`.
  -- You can close a connection with `hClose chandle`.
  hPutStrLn chandle $ "Hi process " ++ show neighbour ++ "! I'm process " ++ show me ++ " and you are my neighbour."
  putStrLn "I sent a message to the neighbour"
  message <- hGetLine chandle
  putStrLn $ "Neighbour send a message back: " ++ show message
  atomically $ addtotable r neighbour
  atomically $ addToHandleTable h neighbour chandle


-- funtion to handle input
-- we moeten er op deze plaats voor zien de zorgen dat een functie word aangeroepen voor het printen van de tabel 
inputHandler :: Node -> IO ()
inputHandler n@(Node {routingtable = r, handletable = h}) = do
  com <- getLine
  case (com) of
    ("R") -> do 
      -- sendmessage 1102 1100 "sterf"
      putStrLn $ "Command R"
      inputHandler n
    ("B") -> do 
      putStrLn $ "Command B"
      printtabel <- atomically $ readTMVar r
      putStrLn $ show printtabel
      inputHandler n
    ("C") -> do 
      putStrLn $ "Command C"
      printtabel <- atomically $ readTMVar h
      putStrLn $ show printtabel
      inputHandler n
    ("D") -> do 
      putStrLn $ "Command D"
      inputHandler n
    (_) -> do
      putStrLn $ "wrong input"
      inputHandler n

-- Needs improvement       
inputParser :: String -> (String, Int, String)
inputParser text | length split < 2 = (com, 1, "") -- the 1 is an placeholder
        | length split < 3 = (com, port, "")
        | otherwise = (com, port, message)
  where
    split = words text
    com = head split
    port = read (split !! 1) :: Int
    message = last split




-- Test cases
-- stack run -- 1100 1101
-- stack run -- 1101 1100 1102
-- stack run -- 1102 1101 
