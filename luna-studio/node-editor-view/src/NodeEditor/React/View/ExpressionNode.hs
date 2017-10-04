{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ExpressionNode where

import           Common.Prelude
import qualified Data.Aeson                                           as Aeson
import qualified Data.HashMap.Strict                                  as HashMap
import qualified Data.Map.Lazy                                        as Map
import           Data.Matrix                                          (Matrix)
import           Data.Set                                             (Set)
import qualified Data.Set                                             as Set
import qualified Data.Text                                            as Text
import qualified JS.Mount                                             as Mount
import qualified JS.UI                                                as UI
import           LunaStudio.Data.Matrix                               (showNodeMatrix, showNodeTranslate)
import qualified LunaStudio.Data.MonadPath                            as MonadPath
import qualified NodeEditor.Event.Mouse                               as Mouse
import qualified NodeEditor.Event.UI                                  as UI
import qualified NodeEditor.React.Event.Node                          as Node
import qualified NodeEditor.React.Event.Visualization                 as Visualization
import qualified NodeEditor.React.Model.Field                         as Field
import           NodeEditor.React.Model.Node.ExpressionNode           (ExpressionNode, NodeLoc, Subgraph, countArgPorts, countOutPorts,
                                                                      isAnyPortHighlighted, isCollapsed, returnsError)
import qualified NodeEditor.React.Model.Node.ExpressionNode           as Node
import qualified NodeEditor.React.Model.Node.ExpressionNodeProperties as Prop
import           NodeEditor.React.Model.Port                          (isAll, isInPort, isSelf, withOut)
import qualified NodeEditor.React.Model.Port                          as Port
import           NodeEditor.React.Model.Searcher                      (Searcher)
import qualified NodeEditor.React.Model.Searcher                      as Searcher
import           NodeEditor.React.IsRef                               (IsRef, dispatch)
import           NodeEditor.React.View.ColorizedExpression            (colorizedExpression_)
import           NodeEditor.React.View.ExpressionNode.NodeValue       (nodeValue_)
import           NodeEditor.React.View.ExpressionNode.Properties      (nodeProperties_)
import           NodeEditor.React.View.Field                          (multilineField_)
import           NodeEditor.React.View.Monad                          (monads_)
import           NodeEditor.React.View.Plane                          (planeMonads_)
import           NodeEditor.React.View.Port                           (argumentConstructor_, portExpanded_, port_)
import           NodeEditor.React.View.Searcher                       (searcher_)
import           NodeEditor.React.View.Style                          (errorMark_, selectionMark_)
import qualified NodeEditor.React.View.Style                          as Style
import           React.Flux
import qualified React.Flux                                           as React


name, objNameBody, objNamePorts, objNameDynStyles :: JSString
name             = "node"
objNameBody      = "node-body"
objNamePorts     = "node-ports"
objNameDynStyles = "node-dynstyle"

prefixNode :: JSString -> JSString
prefixNode = Mount.prefix . ("node-" <>)

nameLabelId :: JSString
nameLabelId = Mount.prefix "focus-nameLabel"

focusNameLabel :: IO ()
focusNameLabel = UI.focus nameLabelId

handleMouseDown :: IsRef ref => ref -> NodeLoc -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref nodeLoc e m =
    if Mouse.withoutMods m Mouse.leftButton || Mouse.withShift m Mouse.leftButton
    then stopPropagation e : dispatch ref (UI.NodeEvent $ Node.MouseDown m nodeLoc)
    else []

nodeName_ :: IsRef ref => ref -> NodeLoc -> Maybe Text -> Maybe Bool -> Maybe Searcher -> ReactElementM ViewEventHandler ()
nodeName_ ref nl name' visualizationVisible mayS = React.viewWithSKey nodeName  "node-name" (ref, nl, name', visualizationVisible, mayS) mempty

nodeName :: IsRef ref => ReactView (ref, NodeLoc, Maybe Text, Maybe Bool, Maybe Searcher)
nodeName = React.defineView "node-name" $ \(ref, nl, name', mayVisualizationVisible, mayS) -> do
    let regularHandlersAndElem = ( [onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditName nl)]
                                 , elemString . convert $ fromMaybe def name' )
        (handlers, nameElement) = flip (maybe regularHandlersAndElem) mayS $ \s -> case s ^. Searcher.mode of
            Searcher.NodeName snl _ -> if snl /= nl then regularHandlersAndElem else ([], searcher_ ref s)
            _                       -> regularHandlersAndElem
    div_
        ([ "className" $= Style.prefixFromList ["node__name", "noselect"]
        , "key" $= "nodeName"
        ] <> handlers) $ do
        div_
            [ "className" $= Style.prefix "node__name--positioner"
            ] $ do
            nameElement
            withJust mayVisualizationVisible $ \isVisualization ->
                svg_
                    [ "key"       $= "ctrlSwitch"
                    , "className" $= Style.prefix "ctrl-icon"
                    , "xmlns"     $= "http://www.w3.org/2000/svg"
                    , "viewBox"   $= "0 0 24 24"
                    , onDoubleClick $ \e _ -> [stopPropagation e]
                    , onClick       $ \_ _ -> dispatch ref $ UI.VisualizationEvent $ Visualization.ToggleVisualizations nl
                    ] $ if isVisualization
                        then path_ [ "d" $= Style.iconEyeDisabled ] mempty
                        else path_ [ "d" $= Style.iconEye         ] mempty


nodeExpression_ :: IsRef ref => ref -> NodeLoc -> Text -> Maybe Searcher -> ReactElementM ViewEventHandler ()
nodeExpression_ ref nl expr mayS = React.viewWithSKey nodeExpression "node-expression" (ref, nl, expr, mayS) mempty

nodeExpression :: IsRef ref => ReactView (ref, NodeLoc, Text, Maybe Searcher)
nodeExpression = React.defineView "node-expression" $ \(ref, nl, expr, mayS) -> do
    let isLong = Text.length expr > 64

        regularHandlersAndElem  = ( [onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nl)]
                                  , colorizedExpression_ expr )
        (handlers, nameElement) = flip (maybe regularHandlersAndElem) mayS $ \s -> case s ^. Searcher.mode of
            Searcher.Node snl _ _ -> if snl /= nl then regularHandlersAndElem else ([], searcher_ ref s)
            _                     -> regularHandlersAndElem
    div_
        (
        [ "className" $= Style.prefixFromList (["node__expression", "noselect"] <> (if isLong then ["node__expression--long"] else []))
        , "key"       $= "nodeExpression" ] <> handlers
        ) nameElement

node_ :: IsRef ref => ref -> ExpressionNode -> Bool -> Maybe Searcher -> Set NodeLoc -> ReactElementM ViewEventHandler ()
node_ ref model performingConnect s relatedNodesWithVis =
    React.viewWithSKey node (jsShow $ model ^. Node.nodeId) (ref, model, performingConnect, s, relatedNodesWithVis) mempty

node :: IsRef ref => ReactView (ref, ExpressionNode, Bool, Maybe Searcher, Set NodeLoc)
node = React.defineView name $ \(ref, n, performingConnect, maySearcher, relatedNodesWithVis) -> case n ^. Node.mode of
    Node.Expanded (Node.Function fs) -> nodeContainer_ ref performingConnect maySearcher relatedNodesWithVis $ Map.elems fs
    _ -> do
        let nodeId        = n ^. Node.nodeId
            nodeLoc       = n ^. Node.nodeLoc
            nodeLimit     = 10000::Int
            zIndex        = n ^. Node.zPos
            z             = zIndex + 11::Int --if isCollapsed n then zIndex else zIndex + nodeLimit
            hasSelf       = any (\p -> (Port.isSelf $ p ^. Port.portId) && (not $ Port.isInvisible p)) $ Node.inPortsList n
            hasAlias      = any (Port.isAlias . (^. Port.portId)) $ Node.inPortsList n
            mayVisVisible = const (n ^. Node.visualizationsEnabled) <$> n ^. Node.defaultVisualizer
            showValue     = not $ n ^. Node.visualizationsEnabled && Set.member nodeLoc relatedNodesWithVis
            expression    = n ^. Node.expression
            highlight     = if n ^. Node.isMouseOver && (not performingConnect || not (isAnyPortHighlighted n)) then ["hover"] else []
                        --  && (n ^. Node.argConstructorMode /= Port.Highlighted)
                        --  && (not $ any Port.isHighlighted (inPortsList n))
                        --  && (not $ any Port.isHighlighted (outPortsList n)) then ["hover"] else []
            ifPortConstructor = if elem (n ^. Node.argConstructorMode) [Port.Normal, Port.Highlighted] then ["has-port-constructor"] else []
        div_
            [ "key"       $= prefixNode (jsShow nodeId)
            , "id"        $= prefixNode (jsShow nodeId)
            , "className" $= Style.prefixFromList ( [ "node", "noselect", (if isCollapsed n then "node--collapsed" else "node--expanded") ]
                                                                       <> (if returnsError n then ["node--error"] else [])
                                                                       <> (if n ^. Node.isSelected then ["node--selected"] else [])
                                                                       <> (if n ^. Node.isMouseOver && not performingConnect then ["show-ctrl-icon"] else [] )
                                                                       <> (if hasSelf then ["node--has-self"] else ["node--no-self"])
                                                                       <> (if hasAlias then ["node--has-alias"] else ["node--no-alias"])
                                                                       <> highlight
                                                                       <> ifPortConstructor)
            , "style"     @= Aeson.object [ "zIndex" Aeson..= show z ]
            , onMouseDown   $ handleMouseDown ref nodeLoc
            , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeLoc
            , onDoubleClick $ \e _ -> stopPropagation e : (dispatch ref $ UI.NodeEvent $ Node.Enter nodeLoc)
            , onMouseEnter  $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.MouseEnter nodeLoc
            , onMouseLeave  $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.MouseLeave nodeLoc
            ] $ do
            div_
                [ "className" $= Style.prefixFromList [ "node-translate","node__text", "noselect" ]
                , "key"       $= "nodeText"
                ] $ do
                nodeName_ ref nodeLoc (n ^. Node.name) mayVisVisible maySearcher
                nodeExpression_ ref nodeLoc expression maySearcher
            nodeBody_  ref n
            when showValue $ nodeValue_ ref n
            nodePorts_ ref n

nodeDynamicStyles_ :: Matrix Double -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodeDynamicStyles_ camera n = React.viewWithSKey nodeDynamicStyles (jsShow $ n ^. Node.nodeId) (camera, n) mempty

nodeDynamicStyles :: ReactView (Matrix Double, ExpressionNode)
nodeDynamicStyles = React.defineView objNameDynStyles $ \(camera, n) -> style_ $ do
    let nodeId  = n ^. Node.nodeId
        nodePos = n ^. Node.position
    elemString $ "#" <> Mount.mountPoint <> "-node-" <> show nodeId <> " .luna-node-translate--name { transform: " <> showNodeTranslate camera nodePos <> " }"
    elemString $ "#" <> Mount.mountPoint <> "-node-" <> show nodeId <> " .luna-node-translate { transform: "       <> showNodeTranslate camera nodePos <> " }"
    elemString $ "#" <> Mount.mountPoint <> "-node-" <> show nodeId <> " .luna-node-transform { transform: "       <> showNodeMatrix    camera nodePos <> " }"
    elemString $ "#" <> Mount.mountPoint <> "-node-" <> show nodeId <> " path.luna-port__shape { clip-path: url(#port-io-shape-mask-"   <> show nodeId <> ") }"
    elemString $ "#" <> Mount.mountPoint <> "-node-" <> show nodeId <> " path.luna-port__select { clip-path: url(#port-io-select-mask-" <> show nodeId <> ") }"

nodeBody_ :: IsRef ref => ref -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodeBody_ ref model = React.viewWithSKey nodeBody "node-body" (ref, model) mempty

nodeBody :: IsRef ref => ReactView (ref, ExpressionNode)
nodeBody = React.defineView objNameBody $ \(ref, n) -> do
    let nodeLoc = n ^. Node.nodeLoc
    div_
        [ "key"       $= "nodeBody"
        , "id"        $= prefixNode ("body-" <> jsShow (n ^. Node.nodeId))
        , "className" $= Style.prefixFromList [ "node__body", "node-translate" ]
        ] $ do
        errorMark_
        selectionMark_
        case n ^. Node.mode of
            Node.Expanded Node.Controls -> nodeProperties_ ref $ Prop.fromNode n
            Node.Expanded Node.Editor   -> multilineField_ [] "editor"
                $ Field.mk ref (n ^. Node.code)
                & Field.onCancel .~ Just (UI.NodeEvent . Node.SetExpression nodeLoc)
            _                           -> ""

nodePorts_ :: IsRef ref => ref -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodePorts_ ref model = React.viewWithSKey nodePorts objNamePorts (ref, model) mempty

nodePorts :: IsRef ref => ReactView (ref, ExpressionNode)
nodePorts = React.defineView objNamePorts $ \(ref, n) -> do
    let nodeId     = n ^. Node.nodeId
        nodeLoc    = n ^. Node.nodeLoc
        nodePorts' = Node.portsList n
        ports p    = forM_ p $ \port -> port_ ref
                                              nodeLoc
                                              port
                                              (if isInPort $ port ^. Port.portId then countArgPorts n else countOutPorts n)
                                              (withOut isAll (port ^. Port.portId) && countArgPorts n + countOutPorts n == 1)
    svg_
        [ "viewBox"   $= "-20 -20 40 40"
        , "key"       $= "nodePorts"
        , "className" $= Style.prefixFromList [ "node__ports", "node-transform" ]
        ] $ do
        defs_
            [ "key" $= "defs" ] $ do
            clipPath_
                [ "id"  $= fromString ("port-io-shape-mask-" <> show nodeId)
                , "key" $= "portIoShapeMask"
                ] $
                circle_
                    [ "className" $= Style.prefix "port-io-shape-mask"
                    ] mempty
            clipPath_
                [ "id"  $= fromString ("port-io-shape-mask-" <> show nodeId)
                , "key" $= "portIoSelectMask"
                ] $
                circle_
                    [ "className" $= Style.prefix "port-io-select-mask"
                    ] mempty
        g_
            [ "key" $= "nodeTransform"
            ] $ do
            if isCollapsed n then do
                ports $ filter (not . isSelf . (^. Port.portId)) nodePorts'
                ports $ filter       (isSelf . (^. Port.portId)) nodePorts'
            else do
                ports $ filter (      isSelf . (^. Port.portId)) nodePorts'

                forM_  (filter (not . isSelf . (^. Port.portId)) nodePorts') $ portExpanded_ ref nodeLoc
            argumentConstructor_ ref nodeLoc (countArgPorts n) (n ^. Node.argConstructorMode == Port.Highlighted)

nodeContainer_ :: IsRef ref => ref -> Bool -> Maybe Searcher -> Set NodeLoc -> [Subgraph] -> ReactElementM ViewEventHandler ()
nodeContainer_ ref performingConnect maySearcher nodesWithVis subgraphs =
    React.viewWithSKey nodeContainer "node-container" (ref, performingConnect, maySearcher, nodesWithVis, subgraphs) mempty

nodeContainer :: IsRef ref => ReactView (ref, Bool, Maybe Searcher, Set NodeLoc, [Subgraph])
nodeContainer = React.defineView name $ \(ref, performingConnect, maySearcher, nodesWithVis, subgraphs) -> do
    div_
        [ "className" $= Style.prefix "subgraphs"
        ] $ forM_ subgraphs $ \subgraph -> do
        let nodes        = subgraph ^. Node.expressionNodes . to HashMap.elems
            lookupNode m = ( m ^. MonadPath.monadType
                           , m ^. MonadPath.path . to (mapMaybe $ flip HashMap.lookup $ subgraph ^. Node.expressionNodes))
            monads       = map lookupNode $ subgraph ^. Node.monads
        div_
            [ "className" $= Style.prefix "subgraph"
            ] $ do
            forM_ nodes $ \n -> node_ ref
                                      n
                                      performingConnect
                                      (filterOutSearcherIfNotRelated (n ^. Node.nodeLoc) maySearcher)
                                      (Set.filter (Node.containsNode (n ^. Node.nodeLoc)) nodesWithVis)
            planeMonads_ $ monads_ monads

filterOutSearcherIfNotRelated :: NodeLoc -> Maybe Searcher -> Maybe Searcher
filterOutSearcherIfNotRelated _  Nothing  = Nothing
filterOutSearcherIfNotRelated nl (Just s) = if Searcher.isSearcherRelated nl s then return s else Nothing
