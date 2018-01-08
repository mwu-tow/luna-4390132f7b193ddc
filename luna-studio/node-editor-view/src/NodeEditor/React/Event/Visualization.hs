{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}
module NodeEditor.React.Event.Visualization where

import           Common.Data.Event                    (EventName)
import           Common.Prelude
import           LunaStudio.Data.NodeLoc              (NodeLoc)
import           LunaStudio.Data.NodeValue            (VisualizationId, VisualizerName)
import           NodeEditor.React.Model.Visualization (VisualizationParent, _Node)


data Event = Focus                VisualizationParent VisualizationId
           | SelectVisualizer     VisualizationParent VisualizationId VisualizerName
           | ToggleVisualizations VisualizationParent
           deriving (Show, Generic, NFData, Typeable)

instance EventName Event

nodeLoc :: Getter Event (Maybe NodeLoc)
nodeLoc = to nodeLoc' where
    nodeLoc' (Focus                vp _)   = vp ^? _Node
    nodeLoc' (SelectVisualizer     vp _ _) = vp ^? _Node
    nodeLoc' (ToggleVisualizations vp)     = vp ^? _Node
