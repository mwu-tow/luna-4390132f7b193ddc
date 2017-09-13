{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ExpressionNode.Properties where

import           Common.Prelude
import           NodeEditor.React.Model.App                           (App)
import           NodeEditor.React.Model.Node.ExpressionNodeProperties (NodeProperties)
import qualified NodeEditor.React.Model.Node.ExpressionNodeProperties as Prop
import           NodeEditor.React.Store                               (Ref)
import           NodeEditor.React.View.PortControl                    (portControl_, portLabel_)
import qualified NodeEditor.React.View.Style                          as Style
import           React.Flux
import qualified React.Flux                                           as React

objName :: JSString
objName = "node-properties"

nodeProperties :: ReactView (Ref App, NodeProperties)
nodeProperties = React.defineView objName $ \(ref, prop) -> do
    let nodeLoc    = prop ^. Prop.nodeLoc
        ports      = if prop ^. Prop.isExpanded && null (Prop.inPortsList prop) then maybeToList $ prop ^? Prop.inPortAt [] else Prop.inPortsList prop
        controls p = div_
                        [ "className" $= Style.prefix "node__control"
                        ] $ do
                        portLabel_ p
                        portControl_ ref nodeLoc p
    div_
        [ "key"       $= "controls"
        , "className" $= Style.prefixFromList [ "node__controls", "noselect" ]
        ] $ do
        div_
           [ "key"       $= "label-self"
           , "className" $= Style.prefixFromList [ "node__label", "node__label--self" ]
           ] $ elemString "self"
        forM_ ports $ controls

nodeProperties_ :: Ref App -> NodeProperties -> ReactElementM ViewEventHandler ()
nodeProperties_ ref prop = React.viewWithSKey nodeProperties objName (ref, prop) mempty
