{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes       #-}
module NodeEditor.Action.Port.Highlight
    ( handleMouseEnter
    , handleMouseLeave
    ) where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import qualified Data.Set                                   as Set
import           LunaStudio.Data.Port                       (AnyPortId (InPortId', OutPortId'))
import           LunaStudio.Data.PortRef                    (AnyPortRef (..), nodeLoc, portId)
import           NodeEditor.Action.State.Action             (runningActions)
import           NodeEditor.Action.State.Model              (calculatePortMode)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, modifyExpressionNode, modifyOutputNode)
import           NodeEditor.React.Model.Node.ExpressionNode (argConstructorMode, hasPort)
import           NodeEditor.React.Model.IsNode              (inPortAt, outPortAt, HasPorts)
import           NodeEditor.React.Model.Port                (Mode (..), mode, AnyPortId)
import           NodeEditor.State.Action                    (actionsBlockingPortHighlight)
import           NodeEditor.State.Global                    (State)

portModeLens :: HasPorts n => AnyPortId -> Traversal' n Mode
portModeLens = \case
    OutPortId' outpid -> outPortAt outpid . mode
    InPortId'  inpid  -> inPortAt  inpid  . mode

handleMouseEnter :: AnyPortRef -> Command State ()
handleMouseEnter portRef = do
    actions <- Set.fromList <$> runningActions
    let nl  = portRef ^. nodeLoc
        pid = portRef ^. portId
        updateMode m = case m of
            Normal         -> Highlighted
            Invisible      -> Invisible
            Inactive       -> Inactive
            TypeNotMatched -> Highlighted
            Highlighted    -> Highlighted
            Moved pos      -> Moved pos
            NameEdit       -> NameEdit
        notBlocked = Set.null (Set.intersection actions actionsBlockingPortHighlight)
    when notBlocked $ do
        modifyExpressionNode nl $ do
            n <- get
            let portModeLens' = if hasPort pid n then portModeLens pid else argConstructorMode 
            portModeLens' %= updateMode
        modifyOutputNode nl $ portModeLens pid %= updateMode

handleMouseLeave :: AnyPortRef -> Command State ()
handleMouseLeave portRef = do
    let nl  = portRef ^. nodeLoc
        pid = portRef ^. portId
    mayPortMode <- getExpressionNode nl >>= mapM (`calculatePortMode` pid)
    withJust mayPortMode $ \m -> do
        modifyExpressionNode nl $ do
            n <- get
            let portModeLens' = if hasPort pid n then portModeLens pid else argConstructorMode
            portModeLens' .= m
    modifyOutputNode nl $ portModeLens pid .= Normal
