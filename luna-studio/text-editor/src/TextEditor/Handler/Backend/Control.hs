module TextEditor.Handler.Backend.Control
    ( handle
    ) where

import           Common.Action.Command   (Command)
import           Common.Prelude
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import qualified TextEditor.Action.Batch
import           TextEditor.Error.Error
import qualified TextEditor.Event.Batch  as Batch
import           TextEditor.Event.Event  (Event (Batch))
import           TextEditor.State.Global (State)



handle :: Event -> Maybe (Command State ())
handle (Batch (Batch.EmpireStarted _)) = Just $
    openedFile <- activeLocation
	liftIO $ putStrLn "textedit" FUUUCK
    withJust openedFile $ \gl -> do
    	liftIO $ putStrLn "textedit" >> print gl
    	Batch.openFile $ gl ^. GraphLocation.filePath
    -- error "Server crashed." -- could have done that more politely, but… let it crash

handle _ = Nothing
