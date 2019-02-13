{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.React.Model.Layout where

import           Common.Prelude
import qualified Data.Matrix                          as Matrix
import           LunaStudio.Data.CameraTransformation (CameraTransformation, logicalToScreen, screenToLogical)
import           LunaStudio.Data.Position             (Position, move, vector)
import qualified LunaStudio.Data.Position             as Position
import           LunaStudio.Data.ScreenPosition       (ScreenPosition (ScreenPosition))
import qualified LunaStudio.Data.ScreenPosition       as ScreenPosition
import           LunaStudio.Data.Size                 (Size)
import qualified LunaStudio.Data.Size                 as Size
import           LunaStudio.Data.Vector2              (Vector2 (Vector2), scalarProduct, x, y)
import           NodeEditor.React.Model.Constants     (gridSize)
import           NodeEditor.React.Model.Port          (InPort, OutPort, getPositionInSidebar, portId)
import           NodeEditor.React.Model.Sidebar       (InputSidebar, OutputSidebar, inputSidebarPosition, inputSidebarSize,
                                                       outputSidebarPosition, portPositionInInputSidebar, portPositionInOutputSidebar)

data Layout = Layout
        { _screenTransform :: CameraTransformation
        , _scene           :: Maybe Scene
        } deriving (Default, Eq, Generic, Show)

data Scene = Scene
        { _position      :: ScreenPosition
        , _size          :: Size
        , _inputSidebar  :: Maybe InputSidebar
        , _outputSidebar :: Maybe OutputSidebar
        } deriving (Default, Eq, Generic, Show)

makeLenses ''Layout
makeLenses ''Scene

screenCenter :: Getter Scene ScreenPosition
screenCenter = to $ (ScreenPosition . flip scalarProduct 0.5 . view Size.vector) . view size

translateToWorkspace :: ScreenPosition -> ScreenPosition -> CameraTransformation -> Position
translateToWorkspace pos' nodeEditorCenter screenTransform' =
    let pos             = pos' - nodeEditorCenter
        transformMatrix = screenTransform' ^. screenToLogical
        posMatrix       = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace  = Matrix.multStd2 posMatrix transformMatrix
    in Position.fromDoubles (Matrix.getElem 1 1 posInWorkspace) (Matrix.getElem 1 2 posInWorkspace)

translateToScreen :: Position -> ScreenPosition -> CameraTransformation -> ScreenPosition
translateToScreen pos nodeEditorCenter screenTransform' =
    let transformMatrix = screenTransform' ^. logicalToScreen
        posMatrix       = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace  = Matrix.multStd2 posMatrix transformMatrix
    in (ScreenPosition.fromDoubles (Matrix.getElem 1 1 posInWorkspace) (Matrix.getElem 1 2 posInWorkspace)) + nodeEditorCenter

inputSidebarPortPosition :: OutPort -> Layout -> Maybe Position
inputSidebarPortPosition p layout = case layout ^? scene . traverse . inputSidebar . traverse of
    Just inputSidebar' ->
        let pid = p ^. portId
            shift = inputSidebar' ^. inputSidebarPosition . vector + Vector2 0 gridSize
            siz   = inputSidebar' ^. inputSidebarSize
            pos   = ScreenPosition . view ScreenPosition.vector . move shift $
                maybe (portPositionInInputSidebar siz pid) id (getPositionInSidebar p)
        in Just $ translateToWorkspace pos (fromMaybe def $ layout ^? scene . _Just . screenCenter) (layout ^. screenTransform)
    _ -> Nothing

outputSidebarPortPosition :: InPort -> Layout -> Maybe Position
outputSidebarPortPosition p layout = case layout ^? scene . traverse . outputSidebar . traverse of
    Just outputSidebar' ->
        let pid = p ^. portId
            shift = outputSidebar' ^. outputSidebarPosition . vector + Vector2 0 gridSize
            pos   = ScreenPosition . view ScreenPosition.vector . move shift $
                maybe (portPositionInOutputSidebar pid) id (getPositionInSidebar p)
        in Just $ translateToWorkspace pos (fromMaybe def $ layout ^? scene . _Just . screenCenter) (layout ^. screenTransform)
    _ -> Nothing
