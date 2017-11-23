module LunaStudio.API.Graph.Result where

import           Data.Aeson.Types           (FromJSON, ToJSON)
import           Data.Binary                (Binary)
import           LunaStudio.Data.Connection (ConnectionId)
import           LunaStudio.Data.Error      (Error, GraphError)
import           LunaStudio.Data.Graph      (Graph)
import           LunaStudio.Data.Node       (NodeId)
import           Prologue


data Result = Result { _removedNodes       :: [NodeId]
                     , _removedConnections :: [ConnectionId]
                     , _graphUpdates       :: Either (Error GraphError) Graph
                     } deriving (Eq, Generic, Show)

makeLenses ''Result
instance Binary   Result
instance NFData   Result
instance FromJSON Result
instance ToJSON   Result

instance Default Result where
    def = Result def def (Right def)
