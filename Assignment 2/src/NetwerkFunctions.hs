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

--recompute


-- Procedure Recompute (v):
--     begin if v = u
--         then begin Du[v] := 0 ; Nbu[v] := local end
--         else begin (* Estimate distance to v *)
--                 d := 1 + min{ndisu[w, v] : w ∈ Neighu} ;
--                 if d < N then
--                     begin Du[v] := d ;
--                         Nbu[v] := w with 1 + ndisu[w, v] = d
--                     end
--                 else begin Du[v] := N ; Nbu[v] := udef end
--             end ;
--         if Du[v] has changed then
--             forall x ∈ Neighu do send h mydist, v, Du[v]i to x
--     end

recompute :: Node -> Port -> STM ()    
recompute n@(Node {nodeID = me, routingtable = r ,neighbourDistanceTable = bnTable}) int = do
    --let oldDistance = getDistanceTo int -- moet dit hebben voor die laatste stap?
    if me == int 
        then return () -- improve
    else do
        bn <- readTVar bnTable
        let (Connection from d too) = getMinDistanceFromNBto bn int -- getMinDistanceFromNBto moet vragen aan alle buren of ze de afstand naar de int doorsturen en daar de laagste van kiezen, portnumber = nummer van de buur
        let newCon = Connection too (d+1) from
        if d + 1 < 999
            then addToRoutingTable r newCon
        else addToRoutingTable r (DConnection too 999 "undef")
    -- if oldDistance /= (d + 1)
    --     then return()
    -- else return() 

--processing received mydist message
--upon failure of channel
--upon repair of channel

-- function to get the min distance to a node from NeighbouDistanceTable
getMinDistanceFromNBto :: NeighbouDistanceTable -> Port -> Connection
getMinDistanceFromNBto ( x@(Connection _ a pa):y@(Connection _ b _):xs) port = 
    if a < b && pa == port
        then getMinDistanceFromNBto (x:xs) port
        else getMinDistanceFromNBto (y:xs) port


--function to add a connection to the routing table
addToRoutingTable :: TMVar Table -> Connection -> STM ()
addToRoutingTable rt con@(Connection to dis via) = do
    table <- takeTMVar rt
    let newList = filter (\(Connection x _ _) -> x /= to) table
    putTMVar rt $ newList ++ [con]
    return ()

--getDistanceToPortFromRoutingTable :: Table -> Port -> IO() 
getDistanceToPortFromRoutingTable rt des = do
    check <- find (\(Connection x _ _) -> x == des) rt
    return()
