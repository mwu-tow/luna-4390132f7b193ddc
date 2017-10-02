module NodeEditor.Action.Basic.Atom where

import           Common.Action.Command                  (Command)
import           Common.Prelude
import           LunaStudio.Data.GraphLocation          (filePath)
import           NodeEditor.Action.State.App            (getWorkspace, modifyApp)
import           NodeEditor.Action.Basic.ProjectManager (getSettings, loadGraph, saveSettings)
import           NodeEditor.Action.State.NodeEditor     (resetApp)
import           NodeEditor.Batch.Workspace             (currentLocation)
import qualified NodeEditor.Batch.Workspace             as Workspace
import           NodeEditor.React.Model.App             (workspace)
import           NodeEditor.State.Global                (State)


setFile :: FilePath -> Command State ()
setFile path = do
    saveSettings
    mayWorkspace <- getWorkspace
    let mayCurrentLocation = mayWorkspace ^? _Just . currentLocation
        mayCurrentFilePath = view filePath <$> mayCurrentLocation
    when (Just path /= mayCurrentFilePath) $ do
        settings <- getSettings
        let newWorkspace = Workspace.mk path
        modifyApp $ workspace ?= newWorkspace
        loadGraph (newWorkspace ^. currentLocation) $ (, settings) <$> mayCurrentLocation

unsetFile :: Command State ()
unsetFile = do
    saveSettings
    modifyApp $ workspace .= def
    resetApp
