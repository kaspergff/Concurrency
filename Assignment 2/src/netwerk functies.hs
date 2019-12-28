

--VARS
-- Neighu :: [Int]                 neighbours of u
-- Du     :: [0..N]                Du [v] estimates d(u,v)
-- NBu    :: [Int]  array of nodes NBU [v] is preffered neighbor of v
-- Ndisu  :: [0..N]                Ndisu[w,v] estimates d(w.v)
--initialization
--recompute
--processing received mydist message
--upon failure of channel
--upon repair of channel

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

recompute :: Node -> Int -> STM ()    
recompute n@(Node {nodeID = me, routingtable = r ,netwerkSize = n, estimatedD = ed}) int = do
    let oldDistance = getDistanceTo int -- moet dit hebben voor die laatste stap?
    if me == int 
        then = return () -- improve
    else do
        (d, portNumber) = getMinDistanceFromNBto int -- getMinDistanceFromNBto moet vragen aan alle buren of ze de afstand naar de int doorsturen en daar de laagste van kiezen, portnumber = nummer van de buur
        if (d + 1 ) < n
            then = do
                updateDistanceTable ed int (d+1)
                addToRoutingTable r int portNumber (d+1)
        else = do
            updateDistance ed int n
            addToRoutingTable r int undef -- dit moet sws nog heel anders ff over
    if oldDistance > (d + 1)
        sendmessage (d+1) -- moet die dus naar alle buren zijn afstand naar de int sturen
    
--getMinDistanceFromNBto :: Int ->  ....
--updateDistanceTable :: nieuwdatatype... -> ....
--addToRoutingTable :: Routingtable -> nieuwdatatype... -> Int -> Int
--updateDistance :: nieuwdatatype -> Int -> Int
--sendmessage :: Int -> Case??? -> StM()/IO()
