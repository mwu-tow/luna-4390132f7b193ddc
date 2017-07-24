module NodeEditor.Action.Basic.ProjectManager where

import           Common.Prelude
import qualified Data.HashMap.Strict                  as HashMap
import qualified JS.Atom                              as Atom
import qualified JS.LocalStorage                      as JS
import           LunaStudio.Data.GraphLocation        (GraphLocation)
import qualified NodeEditor.Action.Batch              as Batch
import           NodeEditor.Action.Camera.Persistence (saveCamera)
import           NodeEditor.Action.Command            (Command)
import           NodeEditor.Action.State.NodeEditor   (getScreenTranform, resetGraph, setGraphStatus)
import           NodeEditor.Batch.Workspace           (currentLocation, uiGraphLocation)
import           NodeEditor.React.Model.NodeEditor    (GraphStatus (GraphLoading))
import           NodeEditor.State.Global              (State, workspace)
import qualified NodeEditor.State.Global              as Global


loadGraph :: GraphLocation -> Bool -> Command State ()
loadGraph location loadModule = do
    resetGraph
    setGraphStatus GraphLoading
    workspace . _Just . currentLocation .= location
    Atom.setActiveLocation location
    saveCurrentLocation
    Batch.getProgram loadModule

navigateToGraph :: GraphLocation -> Command State ()
navigateToGraph location = do
    mayCurrentLoc <- preuse $ workspace . traverse . currentLocation
    saveCamera
    when (mayCurrentLoc /= Just location) $ do
        loadGraph location False

saveCurrentLocation :: Command State ()
saveCurrentLocation =
    withJustM (preuse $ workspace . traverse . uiGraphLocation) $
        liftIO . JS.saveLocation

saveSettings :: Command State ()
saveSettings = do
    print "SAVING"
    mvp    <- use Global.preferedVisualizers
    camera <- getScreenTranform
    Batch.saveSettings mvp HashMap.empty camera
