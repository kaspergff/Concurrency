module NetwerkFunctions where
import Structure  
import Control.Concurrent.STM
import Control.Exception
import System.IO
import Data.List
import Data.Ord (comparing)

recompute :: Node -> Port -> STM (Port,Int)    
recompute (Node {nodeID = me, routingtable = r ,neighbourDistanceTable = bnTable}) int = do
    if me == int 
        then do
            addToRoutingTable r (Connection int 0 (-1))
            return (me,0)
    else do
        bn <- readTVar bnTable
        (Connection from _d too) <- getMinDistanceFromNBto bn int -- getMinDistanceFromNBto moet vragen aan alle buren of ze de afstand naar de int doorsturen en daar de laagste van kiezen, portnumber = nummer van de buur
        let d = _d + 1
        if d < 24 
            then addToRoutingTable r (Connection too (d) from) 
        else addToRoutingTable r (Connection too 24 (-2))
        return (too,d)


getMinDistanceFromNBto :: NeighbourDistanceTable -> Port -> STM (Connection)
getMinDistanceFromNBto tb p = do
    let filterList = filter (\(Connection _ _ t)-> t == p) tb
        sortList   = sortBy (comparing (\(Connection _ dis _)->dis)) filterList

    if length sortList > 0
        then return (head sortList)
    else return (Connection (-2) 24 p)



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

sendmystatusmessage (Node {handletable = h,nodeID = id'}) = do
    h' <- atomically $ readTVar h
    let receivers = map snd h'
    let message = "Mystatus "
    let justreceivers = map (\x -> (Just x)) receivers
    mapM_ (flip sendmessage message ) justreceivers 


