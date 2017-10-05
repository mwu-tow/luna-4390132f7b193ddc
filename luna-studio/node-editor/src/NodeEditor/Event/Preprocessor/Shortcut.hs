module NodeEditor.Event.Preprocessor.Shortcut where

import           React.Flux                      (KeyboardEvent)

import           Common.Prelude
import           NodeEditor.Event.Event          (Event (Shortcut, UI))
import           NodeEditor.Event.KeyMap         (handleKeyApp, handleKeySearcher)
import qualified NodeEditor.Event.Keys           as Keys
import           NodeEditor.Event.Shortcut       (Command (..))
import qualified NodeEditor.Event.Shortcut       as Shortcut
import           NodeEditor.Event.UI             (UIEvent (AppEvent, SearcherEvent))
import qualified NodeEditor.React.Event.App      as App
import qualified NodeEditor.React.Event.Searcher as Searcher
import qualified React.Flux                      as React


process :: Event -> Maybe Event
process (UI (AppEvent      (App.KeyDown      e))) = Shortcut . flip Shortcut.Event def <$> handleKeyApp e
process (UI (SearcherEvent (Searcher.KeyDown e))) = UI . SearcherEvent <$> handleKeySearcher e
process _ = Nothing
