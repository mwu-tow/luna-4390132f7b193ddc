{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.React.Event.Node where

import           Common.Data.Event       (EventName)
import           Common.Prelude
import           LunaStudio.Data.NodeLoc (NodeLoc)
import           React.Flux              (KeyboardEvent, MouseEvent)


data Event = EditExpression                     NodeLoc
           | EditName                           NodeLoc
           | Enter                              NodeLoc
           | MouseDown            MouseEvent    NodeLoc
           | Select               MouseEvent    NodeLoc
           | SetExpression                      NodeLoc Text
           | MouseEnter                         NodeLoc
           | MouseLeave                         NodeLoc
           | ShowFullError                      NodeLoc
            deriving (Show, Generic, NFData, Typeable)

instance EventName Event

nodeLoc :: Getter Event NodeLoc
nodeLoc = to nodeLoc' where
    nodeLoc' (EditExpression   nl)   = nl
    nodeLoc' (EditName         nl)   = nl
    nodeLoc' (Enter            nl)   = nl
    nodeLoc' (MouseDown      _ nl)   = nl
    nodeLoc' (Select         _ nl)   = nl
    nodeLoc' (SetExpression    nl _) = nl
    nodeLoc' (MouseEnter       nl)   = nl
    nodeLoc' (MouseLeave       nl)   = nl
    nodeLoc' (ShowFullError    nl)   = nl
