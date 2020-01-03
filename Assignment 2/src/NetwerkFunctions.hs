module NetwerkFunctions where

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

--VARS
-- Neighu :: [Int]                 neighbours of u
-- Du     :: [0..N]                Du [v] estimates d(u,v)
-- NBu    :: [Int]  array of nodes NBU [v] is preffered neighbor of v
-- Ndisu  :: [0..N]                Ndisu[w,v] estimates d(w.v)
    
-- recompute :: Node -> Port -> IO ()    
-- recompute n@(Node {nodeID = me, routingtable = r ,neighbourDistanceTable = bnTable}) int = do
--     rtable <- atomically $ readTMVar r
--     let oldDistance = getDistanceToPortFromRoutingTable rtable int -- moet dit hebben voor die laatste stap?\
--     --putStrLn $ "mee " ++ show me ++ " " ++ show int
--     if me == int 
--         then do
--             atomically $ addToRoutingTable r (Connection int 0 (-1))
--             --sendmydistmessage n int 0 
--     else do
--         bn <- atomically $ readTMVar bnTable
--         let (Connection from _d too) = getMinDistanceFromNBto bn int -- getMinDistanceFromNBto moet vragen aan alle buren of ze de afstand naar de int doorsturen en daar de laagste van kiezen, portnumber = nummer van de buur
--         --putStrLn $ "DEEEBUG" ++ " Int" ++ " " ++ show int ++ " " ++ " too" ++ show too
--         let d = _d + 1
--         let newCon = Connection too (d) from
--         if d < 999 
--             then do 
--                 atomically $ addToRoutingTable r newCon 
--         else atomically $ addToRoutingTable r (Connection too 999 (-2))
--         if oldDistance /= d
--             then sendmydistmessage n int d               
--         else return() 

recompute :: Node -> Port -> STM (Port,Int)    
recompute n@(Node {nodeID = me, routingtable = r ,neighbourDistanceTable = bnTable}) int = do
    if me == int 
        then do
            addToRoutingTable r (Connection int 0 (-1))
            return (me,0)
    else do
        bn <- readTMVar bnTable
        let (Connection from _d too) = getMinDistanceFromNBto bn int -- getMinDistanceFromNBto moet vragen aan alle buren of ze de afstand naar de int doorsturen en daar de laagste van kiezen, portnumber = nummer van de buur
        --putStrLn $ "DEEEBUG" ++ " Int" ++ " " ++ show int ++ " " ++ " too" ++ show too
        let d = _d + 1
        if d < 999 
            then addToRoutingTable r (Connection too (d) from) 
        else addToRoutingTable r (Connection too 999 (-2))
        return (too,d)

















--processing received mydist message
--upon failure of channel
--upon repair of channel

-- function to get the min distance to a node from NeighbouDistanceTable
getMinDistanceFromNBto :: NeighbourDistanceTable -> Port -> Connection
getMinDistanceFromNBto [x] _ = x
getMinDistanceFromNBto ( x@(Connection _ a pa):y@(Connection _ b _):xs) port = 
    if a < b && pa == port
        then getMinDistanceFromNBto (x:xs) port
        else getMinDistanceFromNBto (y:xs) port


--function to add a connection to the routing table
addToRoutingTable :: TMVar Table -> Connection -> STM ()
addToRoutingTable rt con@(Connection to _ _) = do
    table <- takeTMVar rt
    if (length table) < 1
        then putTMVar rt $ table ++ [con]
    else do
        let newList = filter (\(Connection x _ _) -> x /= to) table
        putTMVar rt $ newList ++ [con]
        return ()


getDistanceToPortFromRoutingTable :: Table -> Port -> Int
getDistanceToPortFromRoutingTable rt des = do
    let check = find (\(Connection x _ _) -> x == des) rt
    case check of
        Just (Connection _ dis _) -> dis
        Nothing -> (999)



--sendmydistmessage :: Node -> Port -> Int ->  [IO ()]
--sendmydistmessage n@(Node {nodeID = id, handletable = h}) to dist = do
sendmydistmessage n@(Node {nodeID = id, handletable = h}) to dist = do
    h' <- atomically $ readTMVar h
    let receivers = map snd h'
    let message = ("Mydist " ++ show id ++ " " ++ show to ++ " " ++ show dist)
    let justreceivers = map (\x -> (Just x)) receivers
    mapM_ (flip sendmessage message ) justreceivers 
    
sendmessage :: Maybe (IO Handle) -> String -> IO ()
sendmessage (Just x) message = do
    x' <- x
    hSetBuffering x' LineBuffering
    hPutStrLn x' $ id message
sendmessage (Nothing) _ = putStrLn $ show  "error message"

sendmystatusmessage n@(Node {nodeID = id, handletable = h}) = do
    h' <- atomically $ readTMVar h
    let receivers = map snd h'
    let message = ("Mystatus " )
    let justreceivers = map (\x -> (Just x)) receivers
    mapM_ (flip sendmessage message ) justreceivers 