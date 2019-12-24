
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
type Node = Int  
--we moeten die tabel gaan zien als een reachability graph
--vanaf nu zijn de connecties gwn lekker een eigen type
data Connection = Connection Node Int Node deriving (Show)
--tabel is een lijst van connecties
type Table = [Connection]





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
  -- create routing tabel
  tabel <- newTMVarIO []
  -- Part 1 Initialisation (Geen idee of dit persee in een apparte thread moet)
  _ <- forkIO $ initialisation me neighbours tabel
  -- -- Part 2 input
  _ <- forkIO $ inputHandler tabel

  threadDelay 1000000000

readCommandLineArguments :: IO (Node, [Node])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, map read neighbours)

portToAddress :: Node -> SockAddr
portToAddress portNumber = SockAddrInet (fromIntegral portNumber) (tupleToHostAddress (127, 0, 0, 1)) -- localhost

connectSocket :: Node -> IO Socket
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
initialisation :: Node -> [Node] -> (TMVar [Connection]) -> IO ()
initialisation _ [] _             = do putStrLn "I have no more neighbours :("
initialisation me (neighbour:xs) tabel = do
  makeConnnection me neighbour tabel
  initialisation me xs tabel

--hier moeten we dus nog voor zorgen dat er nog een distance berekend word en word meegegeven maar das voor later zorg
addtotable :: (TMVar [Connection]) -> Node -> STM ()
addtotable tabel neighbour= do 
  lijst <- takeTMVar tabel
  putTMVar tabel (lijst ++  [(Connection neighbour 1 neighbour)]) 

-- function to write a messsage from node a to b (kunnen we straks mooi gebruiken voor een astractie van de fail en repair messaged ed)
-- string is voor nu het datatype maar kan mis beter een tuple worden van typebericht en bericht of we kunnen een datatype bericht maken dat
-- Fail| repair | message is
-- hij schrijft wel een bericht maar het kan maar zo dat hij constant bezig is met het toevoegen van nieuwe connecties tussen nodes die al geconnect zijn
sendmessage :: Int -> Int -> String -> IO ()
sendmessage from to message = do 
  client <- connectSocket to
  chandle <- socketToHandle client ReadWriteMode
  hPutStrLn chandle $ "Hi process " ++ show to ++ "! I'm process " ++ show from ++ " and i wanted to say" ++ show message

-- function to make a connection between two nodes  
makeConnnection :: Node -> Node -> (TMVar [Connection]) -> IO ()
makeConnnection me neighbour tabel = do 
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
  atomically $ addtotable tabel neighbour


-- funtion to handle input
-- we moeten er op deze plaats voor zien de zorgen dat een functie word aangeroepen voor het printen van de tabel 
inputHandler :: (TMVar [Connection]) -> IO ()
inputHandler tabel = do
  input <- getLine
  let (com, node, message) = inputParser input
  case (com) of
    ("R") -> do 
      -- sendmessage 1102 1100 "sterf"
      putStrLn $ "Command R"
      inputHandler tabel
    ("B") -> do 
      putStrLn $ "Command B"
      printtabel <- atomically $ readTMVar tabel
      putStrLn $ show printtabel
      inputHandler tabel
    ("C") -> do 
      putStrLn $ "Command C"
      inputHandler tabel
    ("D") -> do 
      putStrLn $ "Command D"
      inputHandler tabel
    (_) -> do
      putStrLn $ "wrong input"
      inputHandler tabel

-- Needs improvement       
inputParser :: String -> (String, Node, String)
inputParser text | length split < 2 = (com, 1, "") -- the 1 is an placeholder
        | length split < 3 = (com, port, "")
        | otherwise = (com, port, message)
  where
    split = words text
    com = head split
    port = read (split !! 1) :: Node
    message = last split

