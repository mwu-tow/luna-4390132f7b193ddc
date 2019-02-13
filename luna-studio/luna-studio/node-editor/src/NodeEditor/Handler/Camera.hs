module NodeEditor.Handler.Camera
    ( handle
    ) where

import           Common.Action.Command          (Command)
import           Common.Prelude
import           LunaStudio.Data.Vector2        (Vector2 (Vector2))
import           NodeEditor.Action.Camera       (centerGraph, panCamera, panDown, panDrag, panLeft, panRight, panUp, resetCamera, resetPan,
                                                 resetZoom, startPanDrag, startZoomDrag, stopPanDrag, stopZoomDrag, wheelZoom, zoomDrag,
                                                 zoomIn, zoomOut)
import           NodeEditor.Event.Event         (Event (Shortcut, UI))
import qualified NodeEditor.Event.Mouse         as Mouse
import qualified NodeEditor.Event.Shortcut      as Shortcut
import           NodeEditor.Event.UI            (UIEvent (AppEvent, SidebarEvent))
import qualified NodeEditor.React.Event.App     as App
import qualified NodeEditor.React.Event.Sidebar as Sidebar
import           NodeEditor.State.Action        (Action (continue))
import           NodeEditor.State.Global        (State)
import           NodeEditor.State.Mouse         (mousePosition)
import           React.Flux                     (MouseEvent, wheelDeltaX, wheelDeltaY)


-- TODO[react]: Consider mac trackpad!!!
handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event command _))         = Just $ handleCommand command
handle (UI (AppEvent (App.MouseDown e _)))           = Just $ handleMouseDown e
handle (UI (AppEvent (App.MouseMove e _)))           = Just $ handleMouseMove e
handle (UI (SidebarEvent (Sidebar.MouseMove e _ _))) = Just $ handleMouseMove e
handle (UI (AppEvent (App.MouseUp   _)))             = Just $ continue stopPanDrag >> continue stopZoomDrag
handle (UI (AppEvent (App.Wheel m w)))               = Just $ handleMouseWheel m delta where
    deltaX = fromIntegral $ -(wheelDeltaX w)
    deltaY = fromIntegral $ -(wheelDeltaY w)
    delta  = Vector2 deltaX deltaY
handle _                                             = Nothing


-- TODO consider using state and below approach
-- init = do
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--


handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.CenterGraph -> centerGraph
    Shortcut.PanDown     -> panDown
    Shortcut.PanLeft     -> panLeft
    Shortcut.PanRight    -> panRight
    Shortcut.PanUp       -> panUp
    Shortcut.ResetCamera -> resetCamera
    Shortcut.ResetPan    -> resetPan >> centerGraph
    Shortcut.ResetZoom   -> resetZoom
    Shortcut.ZoomIn      -> zoomIn
    Shortcut.ZoomOut     -> zoomOut
    _                    -> return ()


handleMouseDown :: MouseEvent -> Command State ()
handleMouseDown evt
    | Mouse.withoutMods evt Mouse.rightButton  = startZoomDrag =<< mousePosition evt
    | Mouse.withoutMods evt Mouse.middleButton = startPanDrag  =<< mousePosition evt
    | otherwise                                = return ()

handleMouseMove :: MouseEvent -> Command State ()
handleMouseMove evt
    | Mouse.withoutMods evt Mouse.rightButton  = continue . zoomDrag =<< mousePosition evt
    | Mouse.withoutMods evt Mouse.middleButton = continue . panDrag  =<< mousePosition evt
    | otherwise                                = return ()

handleMouseWheel :: MouseEvent -> Vector2 Double -> Command State ()
handleMouseWheel evt delta
    | Mouse.withoutMods evt Mouse.leftButton = panCamera delta
    | Mouse.withCtrl    evt Mouse.leftButton = flip wheelZoom delta =<< mousePosition evt
    | otherwise                              = return ()
