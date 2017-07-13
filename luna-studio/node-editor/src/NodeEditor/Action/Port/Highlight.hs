{-# LANGUAGE TypeApplications #-}
module NodeEditor.Action.Port.Highlight
    ( handleMouseEnter
    , handleMouseLeave
    ) where

import           Common.Prelude
import qualified Data.Set                                   as Set
import           LunaStudio.Data.Port                       (AnyPortId (InPortId', OutPortId'), InPortIndex (Self))
import           LunaStudio.Data.PortRef                    (AnyPortRef (..), nodeLoc, portId)
import qualified LunaStudio.Data.TypeRep                    as TypeRep
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.Connect                  ()
import           NodeEditor.Action.State.Action             (checkAction, runningActions)
import           NodeEditor.Action.State.Model              (calculatePortMode)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, getPort, modifyExpressionNode)
import           NodeEditor.React.Model.Connection          (toValidEmpireConnection)
import           NodeEditor.React.Model.Node.ExpressionNode (argConstructorMode, hasPort, inPortAt, isCollapsed, outPortAt)
import           NodeEditor.React.Model.Port                (Mode (..), mode, valueType)
import           NodeEditor.State.Action                    (actionsBlockingPortHighlight, connectAction, connectSourcePort)
import           NodeEditor.State.Global                    (State)


handleMouseEnter :: AnyPortRef -> Command State ()
handleMouseEnter portRef = do
    actions <- Set.fromList <$> runningActions
    let notBlocked = Set.null (Set.intersection actions actionsBlockingPortHighlight)
    when notBlocked $ modifyExpressionNode (portRef ^. nodeLoc) $ do
        n <- get
        let pid = portRef ^. portId
            updateMode m = case m of
                Normal         -> Highlighted
                Invisible      -> Invisible
                Inactive       -> Inactive
                TypeNotMatched -> Highlighted
                Highlighted    -> Highlighted
                Moved pos      -> Moved pos
                NameEdit       -> NameEdit
            portModeLens = if not $ hasPort pid n
                then argConstructorMode
                else case pid of
                    OutPortId' outpid -> outPortAt outpid . mode
                    InPortId'  inpid  -> inPortAt  inpid  . mode
        portModeLens %= updateMode


handleMouseLeave :: AnyPortRef -> Command State ()
handleMouseLeave portRef = do
    let nl  = portRef ^. nodeLoc
        pid = portRef ^. portId
    mayPortMode <- getExpressionNode nl >>= mapM (flip calculatePortMode pid)
    withJust (mayPortMode) $ \m -> modifyExpressionNode nl $ do
        portModeLens <- get >>= \n -> do
            return $ if not $ hasPort pid n then argConstructorMode else case pid of
                OutPortId' outpid -> outPortAt outpid . mode
                InPortId'  inpid  -> inPortAt  inpid  . mode
        portModeLens .= m
