module NodeEditor.Handler.Sidebar where

import           Common.Action.Command          (Command)
import           Common.Prelude
import           NodeEditor.Action.Basic        (toggleInputMode, toggleOutputMode)
import qualified NodeEditor.Action.Batch        as Batch
import           NodeEditor.Action.Searcher     (editPortName)
import           NodeEditor.Action.Sidebar      (handleAppMove, handleMouseUp, handleSidebarMove)
import           NodeEditor.Event.Event         (Event (UI))
import           NodeEditor.Event.UI            (UIEvent (AppEvent, SidebarEvent))
import qualified NodeEditor.React.Event.App     as App
import qualified NodeEditor.React.Event.Sidebar as Sidebar
import           NodeEditor.React.Model.Port    (AnyPortRef (OutPortRef'))
import           NodeEditor.State.Action        (Action (continue))
import           NodeEditor.State.Global        (State)


handle :: Event -> Maybe (Command State ())
handle (UI (SidebarEvent (Sidebar.ToggleInputMode   nl)))                         = Just $ toggleInputMode nl
handle (UI (SidebarEvent (Sidebar.ToggleOutputMode  nl)))                         = Just $ toggleOutputMode nl
handle (UI (SidebarEvent (Sidebar.RemovePort        (OutPortRef' portRef))))      = Just $ Batch.removePort portRef
handle (UI (SidebarEvent (Sidebar.AddPort           (OutPortRef' portRef))))      = Just $ Batch.addPort portRef Nothing
handle (UI (AppEvent     (App.MouseMove             evt _)))                      = Just $ handleAppMove evt
handle (UI (SidebarEvent (Sidebar.MouseMove         evt nodeLoc _)))                = Just $ handleSidebarMove evt nodeLoc
handle (UI (AppEvent     (App.MouseUp               evt)))                        = Just $ continue $ handleMouseUp evt
handle (UI (SidebarEvent (Sidebar.EditPortName      portRef)))                    = Just $ editPortName portRef
handle _                                                                          = Nothing
