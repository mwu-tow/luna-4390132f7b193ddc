module NodeEditor.Action.State.Model.ExpressionNode where

import           Common.Prelude
import           Control.Monad                              (filterM)
import qualified JS.Node                                    as JS
import           LunaStudio.Data.Geometry                   (isPointInCircle, isPointInRectangle)
import           LunaStudio.Data.PortRef                    (InPortRef (InPortRef), toAnyPortRef)
import           LunaStudio.Data.Position                   (Position)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.Action             (checkIfActionPerfoming)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodes, getScene, inGraph)
import           NodeEditor.React.Model.Connection          (toValidEmpireConnection)
import           NodeEditor.React.Model.Constants           (nodeRadius)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, hasPort, isCollapsed, nodeId, nodeLoc, nodeLoc,
                                                             position, position, zPos)
import           NodeEditor.React.Model.Port                (AnyPortId (InPortId'), InPortIndex (Self))
import           NodeEditor.State.Action                    (connectSourcePort, penConnectAction)
import           NodeEditor.State.Global                    (State, actions, currentConnectAction)


isPointInNode :: Position -> ExpressionNode -> Command State Bool
isPointInNode p node =
    if isCollapsed node
        then return $ isPointInCircle p (node ^. position, nodeRadius)
        else do
            let nid = node ^. nodeId
            getScene >>= \case
                Just scene -> isPointInRectangle p <$> JS.expandedNodeRectangle scene nid
                Nothing -> return False

getNodeAtPosition :: Position -> Command State (Maybe NodeLoc)
getNodeAtPosition p = do
    nodes <- getExpressionNodes >>= filterM (isPointInNode p)
    if null nodes
        then return Nothing
        else return $ Just $ maximumBy (\node1 node2 -> compare (node1 ^. zPos) (node2 ^. zPos)) nodes ^. nodeLoc



shouldDisplayPortSelf :: ExpressionNode -> Command State Bool
shouldDisplayPortSelf node = do
    let selfId = InPortId' [Self]
    if not $ hasPort selfId node
        then return False
        else do
            let nl = node ^. nodeLoc
            penConnecting    <- checkIfActionPerfoming penConnectAction
            mayConnectAction <- use $ actions . currentConnectAction
            let connectToSelfPossible = isJust . join $
                    (toValidEmpireConnection (toAnyPortRef nl selfId) . view connectSourcePort) <$> mayConnectAction
                isSource = (view connectSourcePort <$> mayConnectAction) == Just (toAnyPortRef nl selfId)
            if (not . isCollapsed $ node) || penConnecting || connectToSelfPossible || isSource
                then return True
                else inGraph $ InPortRef nl [Self]
