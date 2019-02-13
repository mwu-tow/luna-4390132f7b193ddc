module NodeEditor.React.Model.Visualization
    ( module NodeEditor.React.Model.Visualization
    , module X
    ) where

import LunaStudio.Data.Visualization as X (VisualizationId,
                                           VisualizationValue (..))
import LunaStudio.Data.Visualizer    as X (ExternalVisualizers (ExternalVisualizers),
                                           Visualizer (Visualizer),
                                           VisualizerId (VisualizerId),
                                           VisualizerMatcher, VisualizerName,
                                           VisualizerPath, VisualizerType (..),
                                           Visualizers (Visualizers),
                                           errorVisId, externalVisualizers,
                                           getMdVisualizer, internalVisualizers,
                                           librariesVisualizers,
                                           lunaVisualizers,
                                           mapExternalVisualizers,
                                           mapExternalVisualizersM,
                                           mapExternalVisualizersWithKeyM,
                                           mapVisualizers,
                                           mapVisualizersWithKeyM,
                                           placeholderVisId, projectVisualizers,
                                           visualizerId, visualizerName,
                                           visualizerRelPath, visualizerType,
                                           _InternalVisualizer, _LunaVisualizer,
                                           _ProjectVisualizer)

import Common.Prelude

import qualified Data.Map as Map

import Data.Map                (Map)
import IdentityString          (IdentityString)
import LunaStudio.Data.NodeLoc (NodeLoc)


data VisualizationBackup
    = ValueBackup   IdentityString
    | StreamBackup  [IdentityString]
    | MessageBackup Text
    | ErrorBackup   Text
    deriving (Eq)

makePrisms ''VisualizationBackup

data VisualizationsBackupMap = VisualizationsBackupMap
    { _backupMap :: Map NodeLoc VisualizationBackup
    } deriving (Generic)

makeLenses ''VisualizationsBackupMap
instance Eq      VisualizationsBackupMap where _ == _ = True
instance Default VisualizationsBackupMap



data VisualizationMode
    = Default
    | Focused
    | Preview
    | FullScreen
    deriving (Eq, Generic, Show)

makePrisms ''VisualizationMode
instance Default VisualizationMode where def = Default
instance NFData VisualizationMode

data VisualizationStatus
    = Ready
    | Outdated
    deriving (Eq, Generic, Show)

makePrisms ''VisualizationStatus
instance NFData VisualizationStatus

data VisualizationParent
    = Node NodeLoc
    | Searcher
    deriving (Eq, Generic, Show)

makePrisms ''VisualizationParent
instance NFData VisualizationParent

data VisualizerProperties = VisualizerProperties
    { _runningVisualizer    :: Visualizer
    , _selectedVisualizerId :: Maybe VisualizerId
    } deriving (Eq, Generic, Show)

makeLenses ''VisualizerProperties
instance NFData VisualizerProperties

data RunningVisualization = RunningVisualization
    { _visualizationId      :: VisualizationId
    , _visualizationMode    :: VisualizationMode
    , _visualizerProperties :: VisualizerProperties
    } deriving (Eq, Generic, Show)

makeLenses ''RunningVisualization
instance NFData RunningVisualization

data IdleVisualization = IdleVisualization
    { _visualizationStatus      :: VisualizationStatus
    , _idleVisualizerProperties :: VisualizerProperties
    } deriving (Eq, Generic, Show)

makeLenses ''IdleVisualization
instance NFData IdleVisualization

data NodeVisualizations = NodeVisualizations
    { _visualizations     :: Map VisualizationId RunningVisualization
    , _idleVisualizations :: [IdleVisualization]
    , _visualizers        :: Map VisualizerId VisualizerPath
    } deriving (Eq, Generic, Show)

makeLenses ''NodeVisualizations
instance Default NodeVisualizations where def = NodeVisualizations def def def

data VisualizationProperties = VisualizationProperties
    { _visPropNodeLoc        :: NodeLoc
    , _visPropIsNodeExpanded :: Bool
    , _visPropArgPortsNumber :: Int
    , _visPropVisualizers    :: Map VisualizerId VisualizerPath
    , _visPropVisualization  :: RunningVisualization
    } deriving (Eq, Generic, Show)

makeLenses ''VisualizationProperties
instance NFData VisualizationProperties


toIdleVisualization
    :: VisualizationStatus -> RunningVisualization -> IdleVisualization
toIdleVisualization vs = IdleVisualization vs . view visualizerProperties

stopVisualizations :: NodeVisualizations -> NodeVisualizations
stopVisualizations nodeVis = nodeVis
    & visualizations     .~ def
    & idleVisualizations
        .~ (nodeVis ^. idleVisualizations)
        <> (map
            (toIdleVisualization Ready)
            $ Map.elems $ nodeVis ^. visualizations)


awaitingDataMsg, noVisMsg, noDataMsg :: Text
awaitingDataMsg = "AWAITING_DATA"
noVisMsg        = "NO_VIS_FOR_TYPE"
noDataMsg       = "NO_DATA"
