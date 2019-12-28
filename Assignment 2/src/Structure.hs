module Structure where  
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
    handletable  :: (TMVar HandleTable),
    neighbourDistanceTable :: (TMVar NeighbourDistanceTable)
    }  
-- ik wil dit graag
type Port = Int

--we moeten die tabel gaan zien als een reachability graph
--vanaf nu zijn de connecties gwn lekker een eigen type
data Connection = Connection Port Int Port | DConnection Port Int String
instance Show Connection where
    show (Connection a b c) = show a ++ " "++ show b ++ " " ++ show c
    show (DConnection a b c) = show a ++ " "++ show b ++ " " ++ c
data DistanceTo = DistanceTo Port Int

--tabel is een lijst van connecties
type Table = [Connection] 
type NodeHandle = (Int,IO Handle)
type HandleTable = [NodeHandle]

-- lijst met afstand tot alle bekende Nodes
type DistanceTable = [DistanceTo] --Du

type NeighbourDistanceTable  = [Connection]
