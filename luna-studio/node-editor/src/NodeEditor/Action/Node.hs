{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Node
    ( handleNodeDragMouseUp
    , handleMouseEnter
    , handleMouseLeave
    , nodesDrag
    , startNodeDrag
    , editExpression
    , editName
    , snap
    , snapCoord
    , showFullError
    ) where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           LunaStudio.Data.Geometry                   (snap, snapCoord)
import           NodeEditor.Action.Basic                    (isArgConstructorConnectSrc, updateArgConstructorMode)
import           NodeEditor.Action.NodeDrag                 (handleNodeDragMouseUp, nodesDrag, startNodeDrag)
import           NodeEditor.Action.Searcher                 (editExpression)
import           NodeEditor.Action.Searcher                 (editName)
import           NodeEditor.Action.State.NodeEditor         (modifyExpressionNode)
import           NodeEditor.React.Model.Node                (NodeLoc)
import           NodeEditor.React.Model.Node.ExpressionNode (argConstructorMode, isErrorExpanded, isMouseOver)
import qualified NodeEditor.React.Model.Port                as Port
import           NodeEditor.State.Global                    (State)



handleMouseEnter :: NodeLoc -> Command State ()
handleMouseEnter nl = do
    modifyExpressionNode nl $ isMouseOver .= True
    updateArgConstructorMode nl

handleMouseLeave :: NodeLoc -> Command State ()
handleMouseLeave nl = do
    isConnSrc <- isArgConstructorConnectSrc nl
    modifyExpressionNode nl $ do
        isMouseOver        .= False
        argConstructorMode .= if isConnSrc then Port.Highlighted else Port.Invisible

showFullError :: NodeLoc -> Command State ()
showFullError  nl = modifyExpressionNode nl $ isErrorExpanded .= True
