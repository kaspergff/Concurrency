
module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket

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

  -- Part 1 Initialisation (Geen idee of dit persee in een apparte thread moet)
  _ <- forkIO $ initialisation me neighbours

  -- -- Part 2 input
  _ <- forkIO $ inputHandler

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
initialisation :: Int -> [Int] -> IO ()
initialisation _ []              = do putStrLn "I have no more neighbours :("
initialisation me (neighbour:xs) = do
  makeConnnection me neighbour
  initialisation me xs


-- function to make a connection between two nodes  
makeConnnection :: Int -> Int -> IO ()
makeConnnection me neighbour = do 
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


-- -- funtion to handle input 
inputHandler :: IO ()
inputHandler = do
  com <- getLine
  case (com) of
    ("R") -> do 
      putStrLn $ "Command R"
      inputHandler
    ("B") -> do 
      putStrLn $ "Command B"
      inputHandler
    ("C") -> do 
      putStrLn $ "Command C"
      inputHandler
    ("D") -> do 
      putStrLn $ "Command D"
      inputHandler
    (_) -> do
      putStrLn $ "wrong input"
      inputHandler

