module NodeEditor.Action.Basic.AddNode where

import Common.Prelude

import qualified LunaStudio.Data.Node               as Empire
import qualified LunaStudio.Data.Node               as API
import qualified NodeEditor.Action.Batch            as Batch
import qualified NodeEditor.Action.State.NodeEditor as NodeEditor
import qualified NodeEditor.React.Model.NodeEditor  as NE

import Common.Action.Command                (Command)
import Data.Text                            (Text)
import LunaStudio.Data.Geometry             (snap)
import LunaStudio.Data.NodeMeta             (NodeMeta (NodeMeta))
import LunaStudio.Data.Position             (Position)
import NodeEditor.Action.Basic.FocusNode    (focusNode)
import NodeEditor.Action.Basic.SelectNode   (selectNode)
import NodeEditor.Action.State.Model        (calculatePortSelfMode)
import NodeEditor.Action.State.NodeEditor   (addInputNode, addOutputNode,
                                             getSelectedNodes, modifyNodeEditor,
                                             setVisualizationData,
                                             updateNodeVisualizers)
import NodeEditor.Action.UUID               (getUUID)
import NodeEditor.React.Model.Node          (ExpressionNode, InputNode,
                                             NodeLoc (NodeLoc), NodePath,
                                             OutputNode, inPortAt, inPortsList,
                                             nodeLoc)
import NodeEditor.React.Model.Port          (isSelf, mode, portId)
import NodeEditor.React.Model.Visualization (VisualizationBackup (MessageBackup),
                                             awaitingDataMsg)
import NodeEditor.State.Global              (State)


createNode :: NodePath -> Position -> Text -> Bool -> Command State ()
createNode parentPath nodePos expr isDefinition = do
    selected <- getSelectedNodes
    nid      <- getUUID
    let snappedPos  = snap nodePos
        nodeMeta    = NodeMeta snappedPos False def
        connectTo   = if length selected == 1
                      then view nodeLoc <$> listToMaybe selected
                      else Nothing
        empireNode  = Empire.ExpressionNode nid expr isDefinition def def
            def def nodeMeta False
        node        = convert (parentPath, empireNode)
        nl          = NodeLoc parentPath nid
    localAddExpressionNode node
    selectNode nl
    Batch.addNode nl expr nodeMeta connectTo

localAddExpressionNodes :: [ExpressionNode] -> Command State ()
localAddExpressionNodes = mapM_ localAddExpressionNode

localAddExpressionNode :: ExpressionNode -> Command State ()
localAddExpressionNode node = do
    let mayPortSelfId = find isSelf . map (view portId) $ inPortsList node
        updatePortSelf selfPid m = node & inPortAt selfPid . mode .~ m
    node' <- maybe
        (return node)
        (\selfPid -> updatePortSelf selfPid <$> calculatePortSelfMode node)
        mayPortSelfId
    NodeEditor.addExpressionNode node'
    setVisualizationData (node ^. nodeLoc) (MessageBackup awaitingDataMsg) True
    updateNodeVisualizers $ node ^. nodeLoc
    focusNode $ node ^. nodeLoc

localSetInputSidebar :: NodePath -> Maybe API.InputSidebar -> Command State ()
localSetInputSidebar p = \case
    Nothing -> modifyNodeEditor $ NE.inputNode .= def
    Just n  -> localAddInputNode $ convert (p,n)

localSetOutputSidebar :: NodePath -> Maybe API.OutputSidebar -> Command State ()
localSetOutputSidebar p = \case
    Nothing -> modifyNodeEditor $ NE.outputNode .= def
    Just n  -> localAddOutputNode $ convert (p,n)

localAddInputNode :: InputNode -> Command State ()
localAddInputNode = addInputNode

localAddOutputNode :: OutputNode -> Command State ()
localAddOutputNode = addOutputNode
