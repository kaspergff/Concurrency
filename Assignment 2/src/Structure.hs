module Structure where  
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
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
    routingtable :: (TVar Table),
    handletable  :: (TVar HandleTable),
    neighbourDistanceTable :: (TVar NeighbourDistanceTable),
    messageCount :: (TVar Int)
    }  
-- ik wil dit graag
type Port = Int

--we moeten die tabel gaan zien als een reachability graph
--vanaf nu zijn de connecties gwn lekker een eigen type
data Connection = Connection Port Int Port -- | DConnection Port Int String
instance Show Connection where
    show (Connection a b (-2)) = show a ++ " " ++ show b ++ " " ++ "udef"
    show (Connection a b (-1)) = show a ++ " " ++ show b ++ " " ++ "local"
    show (Connection a b c)    = show a ++ " " ++ show b ++ " " ++ show c

instance Eq Connection where
    (Connection a _ b) == (Connection c _ d) = a == c && b == d 

data DistanceTo = DistanceTo Port Int

--tabel is een lijst van connecties
type Table       = [Connection] 
type NodeHandle  = (Int,IO Handle)
type HandleTable = [NodeHandle]

type NeighbourDistanceTable = [Connection]
