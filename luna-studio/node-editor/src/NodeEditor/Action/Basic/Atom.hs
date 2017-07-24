module NodeEditor.Action.Basic.Atom where

import           Common.Prelude
import           LunaStudio.Data.GraphLocation          (filePath)
import           NodeEditor.Action.Basic.ProjectManager (loadGraph)
import           NodeEditor.Action.Batch                (searchNodes)
import           NodeEditor.Action.Camera.Persistence   (saveCamera)
import           NodeEditor.Action.Command              (Command)
import           NodeEditor.Action.State.NodeEditor     (resetGraph)
import           NodeEditor.Batch.Workspace             (currentLocation, nodeSearcherData)
import qualified NodeEditor.Batch.Workspace             as Workspace
import           NodeEditor.State.Global                (State, workspace)


setFile :: FilePath -> Command State ()
setFile path = do
    saveCamera
    mayCurrentFilePath <- preuse $ workspace . traverse . currentLocation . filePath
    mayNsData <- preuse $ workspace . traverse . nodeSearcherData
    when (Just path /= mayCurrentFilePath) $ do
        let newWorkspace = Workspace.mk path
        workspace ?= newWorkspace
        maybe searchNodes (workspace . _Just . nodeSearcherData .=) mayNsData
        loadGraph (newWorkspace ^. currentLocation) True

unsetFile :: Command State ()
unsetFile = do
    saveCamera
    workspace .= def
    resetGraph
