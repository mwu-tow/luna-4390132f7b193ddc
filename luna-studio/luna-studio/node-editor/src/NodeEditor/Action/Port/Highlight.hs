{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
module NodeEditor.Action.Port.Highlight
    ( handleMouseEnter
    , handleMouseLeave
    ) where

import           Common.Action.Command              (Command)
import           Common.Prelude
import qualified Data.Set                           as Set
import           LunaStudio.Data.PortRef            (AnyPortRef (..), nodeLoc, portId)
import           NodeEditor.Action.State.Action     (runningActions)
import           NodeEditor.Action.State.Model      (calculatePortMode)
import           NodeEditor.Action.State.NodeEditor (getNode, modifyExpressionNode, modifyInputNode, modifyOutputNode)
import           NodeEditor.React.Model.IsNode      (portModeAt)
import           NodeEditor.React.Model.Port        (Mode (..))
import           NodeEditor.State.Action            (actionsBlockingPortHighlight)
import           NodeEditor.State.Global            (State)

handleMouseEnter :: AnyPortRef -> Command State ()
handleMouseEnter portRef = do
    actions <- Set.fromList <$> runningActions
    let nl  = portRef ^. nodeLoc
        pid = portRef ^. portId
        updateMode m = case m of
            Normal         -> Highlighted
            TypeNotMatched -> Highlighted
            _              -> m
        notBlocked = Set.null (Set.intersection actions actionsBlockingPortHighlight)
    when notBlocked $ do
        modifyExpressionNode nl $ portModeAt pid %= updateMode
        modifyInputNode      nl $ portModeAt pid %= updateMode
        modifyOutputNode     nl $ portModeAt pid %= updateMode

handleMouseLeave :: AnyPortRef -> Command State ()
handleMouseLeave portRef = do
    let nl  = portRef ^. nodeLoc
        pid = portRef ^. portId
    withJustM (getNode nl >>= mapM (`calculatePortMode` pid)) $ \m -> do
        modifyExpressionNode nl $ portModeAt pid .= m
        modifyInputNode      nl $ portModeAt pid .= m
        modifyOutputNode     nl $ portModeAt pid .= m
