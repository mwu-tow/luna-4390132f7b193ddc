module NodeEditor.Action.State.Graph where

import           Common.Action.Command              (Command)
import           Common.Prelude
import qualified Data.List                          as List
import           LunaStudio.Data.Breadcrumb         (Breadcrumb (Breadcrumb), items)
import           LunaStudio.Data.GraphLocation      (GraphLocation, breadcrumb, filePath)
import           LunaStudio.Data.NodeLoc            (NodePath)
import           NodeEditor.Action.State.NodeEditor (isGraphLoaded)
import           NodeEditor.Action.State.App        (getWorkspace)
import           NodeEditor.Batch.Workspace         (currentLocation)
import           NodeEditor.State.Global            (State)


isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = getWorkspace >>= return . \case
    Just w  -> w ^. currentLocation == location
    Nothing -> False

isCurrentFile :: GraphLocation -> Command State Bool
isCurrentFile location = getWorkspace >>= return . \case
    Just w  -> w ^. currentLocation . filePath == location ^. filePath
    Nothing -> False

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- isGraphLoaded
    return $ icl && igl

inCurrentLocation :: GraphLocation -> (NodePath -> Command State ()) -> Command State ()
inCurrentLocation location action = do
      mayWorkspace <- getWorkspace
      let currentBc = mayWorkspace ^. traverse . currentLocation . breadcrumb . items
      withJust (List.stripPrefix currentBc $ location ^. breadcrumb . items) $
          action . convert . Breadcrumb
