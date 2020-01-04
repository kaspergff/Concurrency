module NetwerkFunctions where

import Structure  
import Control.Concurrent.STM
import System.IO
import Data.List


recompute :: Node -> Port -> STM (Port,Int)    
recompute (Node {nodeID = me, routingtable = r ,neighbourDistanceTable = bnTable}) int = do
    if me == int 
        then do
            addToRoutingTable r (Connection int 0 (-1))
            return (me,0)
    else do
        bn <- readTVar bnTable
        let (Connection from _d too) = getMinDistanceFromNBto bn int -- getMinDistanceFromNBto moet vragen aan alle buren of ze de afstand naar de int doorsturen en daar de laagste van kiezen, portnumber = nummer van de buur
        let d = _d + 1
        if d < 24 
            then addToRoutingTable r (Connection too (d) from) 
        else addToRoutingTable r (Connection too 24 (-2))
        return (too,d)


-- function to get the min distance to a node from NeighbouDistanceTable
getMinDistanceFromNBto :: NeighbourDistanceTable -> Port -> Connection
getMinDistanceFromNBto [x] _ = x
getMinDistanceFromNBto ( x@(Connection _ a pa):y@(Connection _ b _):xs) port = 
    if a < b && pa == port
        then getMinDistanceFromNBto (x:xs) port
        else getMinDistanceFromNBto (y:xs) port

--function to add a connection to the routing table
addToRoutingTable :: TVar Table -> Connection -> STM ()
addToRoutingTable rt con@(Connection to _ _) = do
    table <- readTVar rt
    if (length table) < 1
        then writeTVar rt $ table ++ [con]
    else do
        let newList = filter (\(Connection x _ _) -> x /= to) table
        writeTVar rt $ newList ++ [con]
        return ()

getDistanceToPortFromRoutingTable :: Table -> Port -> Int
getDistanceToPortFromRoutingTable rt des = do
    let check' = find (\(Connection x _ _) -> x == des) rt
    case check' of
        Just (Connection _ dis _) -> dis
        Nothing -> (24)

sendmydistmessage :: Node -> Int -> Int -> IO()
sendmydistmessage node to dist = do
    (message, justreceivers) <- atomically $ getdistdetails node to dist
    mapM_ (flip sendmessage message ) justreceivers 

getdistdetails :: Node -> Int -> Int -> STM (String, [Maybe (IO Handle)])
getdistdetails (Node {nodeID = id', handletable = h}) to dist = do
    h' <- readTVar h
    let receivers = map snd h'
    let message = ("Mydist " ++ show id' ++ " " ++ show to ++ " " ++ show dist)
    let justreceivers = map (\x -> (Just x)) receivers
    return (message,justreceivers)

    
sendmessage :: Maybe (IO Handle) -> String -> IO ()
sendmessage (Just x) message = do
    x' <- x
    hSetBuffering x' LineBuffering
    hPutStrLn x' $ id message
sendmessage (Nothing) _ = putStrLn $ show  "error message"


sendmystatusmessage (Node {handletable = h}) = do
    h' <- atomically $ readTVar h
    let receivers = map snd h'
    let message = ("Mystatus " )
    let justreceivers = map (\x -> (Just x)) receivers
    mapM_ (flip sendmessage message ) justreceivers 

deleteFirst :: (Eq a) => a -> [a] -> [a]
deleteFirst _ [] = [] 
deleteFirst a (b:bc) | a == b    = bc 
                     | otherwise = b : deleteFirst a bc