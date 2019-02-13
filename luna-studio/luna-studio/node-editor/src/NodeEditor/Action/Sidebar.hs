{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module NodeEditor.Action.Sidebar where

import           Common.Action.Command                   (Command)
import           Common.Prelude
import           Control.Monad.Trans.Maybe               (MaybeT (MaybeT), runMaybeT)
import qualified LunaStudio.Data.PortRef                 as PortRef
import           LunaStudio.Data.ScreenPosition          (ScreenPosition, y)
import           NodeEditor.Action.Basic                 (localMovePort, localRemovePort, setInputSidebarPortMode)
import qualified NodeEditor.Action.Batch                 as Batch
import qualified NodeEditor.Action.Connect               as Connect
import           NodeEditor.Action.State.Action          (beginActionWithKey, checkAction, continueActionWithKey, removeActionFromState,
                                                          updateActionWithKey)
import           NodeEditor.Action.State.NodeEditor      (getInputNode, modifyInputNode, modifyOutputNode)
import           NodeEditor.Action.State.Scene           (getInputSidebarSize)
import           NodeEditor.React.Model.Constants        (gridSize)
import           NodeEditor.React.Model.Node.SidebarNode (NodeLoc, SidebarMode (AddRemove, MoveConnect), countProjectionPorts, mode)
import           NodeEditor.React.Model.Port             (AnyPortRef (OutPortRef'), OutPortIndex (Projection), OutPortRef, getPortNumber)
import qualified NodeEditor.React.Model.Port             as Port
import           NodeEditor.React.Model.Sidebar          (portPositionInInputSidebar)
import           NodeEditor.State.Action                 (Action (begin, continue, end, update), Connect, Mode (Click, Drag),
                                                          PortDrag (PortDrag), SidebarAddRemoveMode (SidebarAddRemoveMode),
                                                          connectIsArgumentConstructor, connectMode, connectSourcePort, connectStartPos,
                                                          portDragActPortRef, portDragAction, portDragIsArgumentConstructor, portDragMode,
                                                          portDragPortStartPosInSidebar, portDragStartPortRef, portDragStartPos,
                                                          sidebarAddRemoveModeAction, sidebarAddRemoveModeNodeLoc)
import           NodeEditor.State.Global                 (State)
import           NodeEditor.State.Mouse                  (mousePosition)
import           React.Flux                              (MouseEvent)


instance Action (Command State) PortDrag where
    begin      = beginActionWithKey    portDragAction
    continue   = continueActionWithKey portDragAction
    update     = updateActionWithKey   portDragAction
    end action = if action ^. portDragIsArgumentConstructor
        then do
            void $ localRemovePort $ action ^. portDragActPortRef
            removeActionFromState portDragAction
        else cancelPortDragUnsafe action

instance Action (Command State) SidebarAddRemoveMode where
    begin action = do
        beginActionWithKey sidebarAddRemoveModeAction action
        setSidebarMode (action ^. sidebarAddRemoveModeNodeLoc) AddRemove
    continue     = continueActionWithKey sidebarAddRemoveModeAction
    update       = updateActionWithKey   sidebarAddRemoveModeAction
    end action   = do
        setSidebarMode (action ^. sidebarAddRemoveModeNodeLoc) MoveConnect
        removeActionFromState sidebarAddRemoveModeAction

escapeAddRemoveMode :: SidebarAddRemoveMode -> Command State ()
escapeAddRemoveMode = end

setSidebarMode :: NodeLoc -> SidebarMode -> Command State ()
setSidebarMode nl newMode = do
    modifyInputNode  nl $ mode .= newMode
    modifyOutputNode nl $ mode .= newMode

toggleSidebarMode :: NodeLoc -> Command State ()
toggleSidebarMode nl = do
    let toggleAction :: Maybe SidebarAddRemoveMode -> Command State ()
        toggleAction = maybe (begin $ SidebarAddRemoveMode nl) end
    checkAction sidebarAddRemoveModeAction >>= toggleAction

handleMouseUp :: MouseEvent -> PortDrag -> Command State ()
handleMouseUp evt portDrag = do
    mousePos <- mousePosition evt
    if portDrag ^. portDragMode == Click || mousePos /= portDrag ^. portDragStartPos
        then finishPortDrag portDrag
    else if portDrag ^. portDragMode == Drag && mousePos == portDrag ^. portDragStartPos
        then update $ portDrag & portDragMode .~ Click
    else return ()

handleSidebarMove :: MouseEvent -> NodeLoc -> Command State ()
handleSidebarMove evt nodeLoc = do
    continue $ restorePortDrag nodeLoc
    continue $ handleMove evt

handleAppMove :: MouseEvent -> Command State ()
handleAppMove evt = do
    continue restoreConnect
    continue $ Connect.handleMove evt

startPortDrag :: ScreenPosition -> OutPortRef -> Bool -> Mode -> Command State ()
startPortDrag mousePos portRef isArgumentConstructor mode' = do
    maySuccess <- runMaybeT $ do
        let portId  = portRef ^. PortRef.srcPortId
        portPos <- MaybeT $ fmap2 (`portPositionInInputSidebar` portId) getInputSidebarSize
        lift . setInputSidebarPortMode portRef $ Port.Moved portPos
        lift . begin $ PortDrag mousePos portPos portRef portRef isArgumentConstructor mode'
    when (isNothing maySuccess && isArgumentConstructor) $ void $ localRemovePort portRef

handleMove :: MouseEvent -> PortDrag -> Command State ()
handleMove evt portDrag = do
    mousePos <- mousePosition evt
    let portRef       = portDrag ^. portDragActPortRef
        nodeLoc       = portRef  ^. PortRef.nodeLoc
        portId        = portRef  ^. PortRef.srcPortId
        startPortPos  = portDrag ^. portDragPortStartPosInSidebar
        startMousePos = portDrag ^. portDragStartPos
        newPos        = startPortPos & y +~ mousePos ^. y - startMousePos ^. y
    mayNewPortNum <- case portId of
        (Projection i: _) -> runMaybeT $ do
            node <- MaybeT $ getInputNode nodeLoc
            let newNum = min (countProjectionPorts node - 1) $ max 0 (round $ (newPos ^. y - gridSize) / (gridSize * 2))
            if newNum /= i then return newNum else nothing
        _                          -> $notImplemented
    setInputSidebarPortMode portRef $ Port.Moved newPos
    withJust mayNewPortNum $ \newNum ->
        withJustM (localMovePort portRef newNum) $ \newPortRef ->
            update $ portDrag & portDragActPortRef .~ newPortRef

cancelPortDragUnsafe :: PortDrag -> Command State ()
cancelPortDragUnsafe portDrag = do
    let portRef    = portDrag ^. portDragActPortRef
        orgPortRef = portDrag ^. portDragStartPortRef
        orgPortId  = orgPortRef ^. PortRef.srcPortId
    setInputSidebarPortMode portRef Port.Normal
    when (portRef /= orgPortRef) $
        void $ localMovePort portRef $ getPortNumber orgPortId
    removeActionFromState portDragAction

finishPortDrag :: PortDrag -> Command State ()
finishPortDrag portDrag = do
    let portRef    = portDrag ^. portDragActPortRef
        orgPortRef = portDrag ^. portDragStartPortRef
        portId     = portRef ^. PortRef.srcPortId
        isArgumentConstructor  = portDrag ^. portDragIsArgumentConstructor
    if portRef == orgPortRef then end portDrag else do
        setInputSidebarPortMode portRef Port.Normal
        if isArgumentConstructor
            then Batch.addPort portRef Nothing def
            else Batch.movePort orgPortRef $ getPortNumber portId
        removeActionFromState portDragAction

restoreConnect :: PortDrag -> Command State ()
restoreConnect portDrag = do
    cancelPortDragUnsafe portDrag
    Connect.startConnecting (portDrag ^. portDragStartPos) (OutPortRef' $ portDrag ^. portDragStartPortRef) Nothing (portDrag ^. portDragIsArgumentConstructor) (portDrag ^. portDragMode)

restorePortDrag :: NodeLoc -> Connect -> Command State ()
restorePortDrag nodeLoc connect = when (connect ^. connectSourcePort . PortRef.nodeLoc == nodeLoc) $
    case connect ^. connectSourcePort of
        OutPortRef' sourcePort -> do
            Connect.stopConnectingUnsafe connect
            startPortDrag (connect ^. connectStartPos) sourcePort (connect ^. connectIsArgumentConstructor) (connect ^. connectMode)
        _                      -> return ()
