{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.React.Model.Sidebar where

import           Common.Prelude
import           LunaStudio.Data.Position         (Position, fromDoubles)
import           LunaStudio.Data.ScreenPosition   (ScreenPosition)
import           LunaStudio.Data.Size             (Size)
import           NodeEditor.React.Model.Constants (gridSize)
import           NodeEditor.React.Model.Port      (InPortId, OutPortId, getPortNumber, isSelf)

portHeight, portWidth :: Double
portHeight = gridSize * 2.0
portWidth  = portHeight

data InputSidebar = InputSidebar { _inputSidebarPosition :: ScreenPosition
                                 , _inputSidebarSize     :: Size
                                 } deriving (Default, Eq, Generic, Show)

data OutputSidebar = OutputSidebar { _outputSidebarPosition :: ScreenPosition
                                   , _outputSidebarSize     :: Size
                                   } deriving (Default, Eq, Generic, Show)

makeLenses ''InputSidebar
makeLenses ''OutputSidebar

portPositionInInputSidebar :: Size -> OutPortId -> Position
portPositionInInputSidebar sidebarSize pid = fromDoubles posX posY where
    portNum = getPortNumber pid
    posX    = portWidth/2 --sidebarSize ^. width
    posY    = (fromIntegral portNum) * portHeight + portHeight/2

portPositionInOutputSidebar :: InPortId -> Position
portPositionInOutputSidebar pid = fromDoubles posX posY where
    portNum = getPortNumber pid
    posX    = portWidth/2
    posY    = (fromIntegral $ if isSelf pid then 0 else portNum) * portHeight + portHeight/2
