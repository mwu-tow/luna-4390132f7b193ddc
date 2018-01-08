{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Event.Filter where

import           Common.Data.Event                          (eventName)
import           Common.Prelude
import qualified Data.Text                                  as Text
import           JS.Atom                                    (acceptEvent)
import qualified JS.Event                                   as JS
import           LunaStudio.Data.Port                       (AnyPortId (InPortId', OutPortId'), isSelf)
import qualified LunaStudio.Data.PortRef                    as PortRef
import           NodeEditor.Event.Event                     (Event (UI))
import           NodeEditor.Event.UI                        (UIEvent (NodeEvent, PortEvent, VisualizationEvent))
import qualified NodeEditor.React.Event.Node                as NodeEvent
import qualified NodeEditor.React.Event.Port                as PortEvent
import qualified NodeEditor.React.Event.Visualization       as VisualizationEvent
import qualified NodeEditor.React.Model.App                 as App
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.React.Model.NodeEditor          (getExpressionNode)
import qualified NodeEditor.React.Store.Ref                 as Ref
import           NodeEditor.State.Global                    (State)
import qualified NodeEditor.State.Global                    as Global
import qualified NodeEditor.State.UI                        as UI


toJSEvent :: Event -> State -> IO JS.Event
toJSEvent evt state = JS.Event (Text.pack $ eventName evt) . nodeEditorEvent <$> getNodeEditor where
    getNodeEditor = view App.nodeEditor <$> Ref.get (state ^. Global.ui . UI.app)
    portRef (UI (PortEvent e))          = Just $ e ^. PortEvent.portRef
    portRef _                           = Nothing
    nodeLoc (UI (NodeEvent e))          = Just $ e ^. NodeEvent.nodeLoc
    nodeLoc (UI (VisualizationEvent e)) = e ^. VisualizationEvent.nodeLoc
    nodeLoc e                           = view PortRef.nodeLoc <$> portRef e
    node ne                             = maybe def (flip getExpressionNode ne) $ nodeLoc evt
    getNodeInfo ne                      = (\n -> JS.NodeInfo (n ^. Node.nodeLoc) (fromMaybe def $ n ^. Node.name) (getPortInfo n)) <$> node ne
    toPortInfo n (OutPortId' pid)       = JS.PortInfo $ "OutPort " <> convert (show $ Node.visibleOutPortNumber n pid)
    toPortInfo n (InPortId'  pid)       = JS.PortInfo $ (if isSelf pid then "Self " else "InPort ") <> convert (show $ Node.visibleInPortNumber n pid)
    getPortInfo n                       = toPortInfo n . view PortRef.portId <$> portRef evt
    nodeEditorEvent ne                  = JS.GraphEvent . JS.GraphInfo $ getNodeInfo ne


filterEvents :: State -> Event -> IO State -> IO State
filterEvents state event action = toJSEvent event state >>= \evt -> if acceptEvent evt
    then action
    else return state
