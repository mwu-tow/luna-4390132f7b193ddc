module LunaStudio.Data.NodeMeta where

import           Data.Binary               (Binary)
import           LunaStudio.Data.NodeValue (Visualizer)
import           LunaStudio.Data.Position  (Position)
import           Prologue


data NodeMeta = NodeMeta { _position           :: Position
                         , _displayResult      :: Bool
                         , _selectedVisualizer :: Maybe Visualizer
                         } deriving (Eq, Generic, NFData, Ord, Show)

makeLenses ''NodeMeta
instance Binary NodeMeta

instance Default NodeMeta where
    def = NodeMeta def False def
