module NetwerkFunctions where
import Structure  
import Control.Concurrent.STM
import Control.Exception
import System.IO
import Data.List
import Control.Monad (when)


-- recompute function
recompute :: Node -> Port -> STM (Port,Int,String)    
recompute (Node {nodeID = me, routingtable = r ,neighbourDistanceTable = bnTable}) int = do
    --begin if v = u
        
    if me == int 
        then do
            --then begin Du[v] := 0 ; Nbu[v] := local end
            addToRoutingTable r (Connection int 0 (-1))
            return (me,0,"local")
    else do
        bn <- readTVar bnTable
        --d := 1 + min{ndisu[w, v] : w ∈ Neighu}
        (Connection from _d too) <- getMinDistanceFromNBto bn int -- getMinDistanceFromNBto moet vragen aan alle buren of ze de afstand naar de int doorsturen en daar de laagste van kiezen, portnumber = nummer van de buur
        let d = _d + 1
        if d < 24 
            then do         -- Du[v] := d ;
                -- Nbu[v] := w with 1 + ndisu[w, v] = d
                addToRoutingTable r (Connection too d from) 
                return (too,d,show from)
        else do
            addToRoutingTable r (Connection too 24 (-2))
            return (too,d,"udef")
        

-- function to get min dis of nb to port
getMinDistanceFromNBto :: NeighbourDistanceTable -> Port -> STM Connection
getMinDistanceFromNBto tb p = do
    let filterList = filter (\(Connection _ _ t)-> t == p) tb
        sortList   = sortOn (\(Connection _ dis _)->dis) filterList

    if not (null sortList) 
        then return (head sortList)
    else return (Connection (-2) 24 p)



--function to add a connection to the routing table
addToRoutingTable :: TVar Table -> Connection -> STM ()
addToRoutingTable rt con@(Connection to dis _) = do
     
    table <- readTVar rt
    if dis > 22
        then do
            let newList = filter (\(Connection x _ _) -> x /= to) table
            writeTVar rt $ newList
            return ()
    else do
        let newList = filter (\(Connection x _ _) -> x /= to) table
        writeTVar rt $ newList ++ [con]
        return ()
-- get dis from routing table
getDistanceToPortFromRoutingTable :: Table -> Port -> Int
getDistanceToPortFromRoutingTable rt des = do
    let check' = find (\(Connection x _ _) -> x == des) rt
    case check' of
        Just (Connection _ dis _) -> dis
        Nothing -> 24
-- send dist message
sendMyDistMessage :: Node -> Int -> Int -> IO()
sendMyDistMessage node to dist = do
    (message, justreceivers) <- atomically $ getDistDetails node to dist
    mapM_ (flip sendMessage message ) justreceivers 

getDistDetails :: Node -> Int -> Int -> STM (String, [Maybe (IO Handle)])
getDistDetails (Node {nodeID = id', handletable = h}) to dist = do
    h' <- readTVar h
    let receivers = map snd h'
    let message = "Mydist " ++ show id' ++ " " ++ show to ++ " " ++ show dist
    let justreceivers = map Just receivers
    return (message,justreceivers)

    
sendMessage :: Maybe (IO Handle) -> String -> IO ()
sendMessage (Just x) message = do
    x' <- x
    hSetBuffering x' LineBuffering
    hPutStrLn x' $ message
sendMessage Nothing _ = print  "error message"

sendMyStatusMessage :: Node -> IO()
sendMyStatusMessage (Node {handletable = h}) = do
    h' <- atomically $ readTVar h
    let receivers = map snd h'
    let message = "Mystatus "
    let justreceivers = map Just receivers
    mapM_ (flip sendMessage message ) justreceivers 


