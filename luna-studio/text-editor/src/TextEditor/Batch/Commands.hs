module TextEditor.Batch.Commands where

import           Common.Batch.Connector.Connection (Message (Message), sendRequest)
import           Common.Prelude
import           Data.UUID.Types                   (UUID)
import qualified LunaStudio.API.Atom.CloseFile     as CloseFile
import qualified LunaStudio.API.Atom.FileChanged   as FileChanged
import qualified LunaStudio.API.Atom.GetBuffer     as GetBuffer
import qualified LunaStudio.API.Atom.IsSaved       as IsSaved
import qualified LunaStudio.API.Atom.OpenFile      as OpenFile
import qualified LunaStudio.API.Atom.SaveFile      as SaveFile
import qualified LunaStudio.API.Atom.SetProject    as SetProject
import qualified LunaStudio.API.Atom.Substitute    as Substitute
import           LunaStudio.Data.GraphLocation     (GraphLocation)
import           LunaStudio.Data.Point             (Point)

-- Atom requests --

closeFile :: FilePath -> UUID -> Maybe UUID -> IO ()
closeFile path uuid guiID = sendRequest $ Message uuid guiID $ CloseFile.Request path

fileChanged :: FilePath -> UUID -> Maybe UUID -> IO ()
fileChanged path uuid guiID = sendRequest $ Message uuid guiID $ FileChanged.Request path

getBuffer :: FilePath -> Maybe [(Int, Int)] -> UUID -> Maybe UUID -> IO ()
getBuffer path maybeSpan uuid guiID = sendRequest $ Message uuid guiID $ GetBuffer.Request path maybeSpan

isSaved :: FilePath -> UUID -> Maybe UUID -> IO ()
isSaved path uuid guiID = sendRequest $ Message uuid guiID $ IsSaved.Request path

openFile :: FilePath -> UUID -> Maybe UUID -> IO ()
openFile path uuid guiID = sendRequest $ Message uuid guiID $ OpenFile.Request path

saveFile :: FilePath -> UUID -> Maybe UUID -> IO ()
saveFile path uuid guiID = sendRequest $ Message uuid guiID $ SaveFile.Request path

setProject :: FilePath -> UUID -> Maybe UUID -> IO ()
setProject rootPath uuid guiID = sendRequest $ Message uuid guiID $ SetProject.Request rootPath

substitute :: GraphLocation -> Point -> Point -> Text -> Maybe Point -> UUID -> Maybe UUID -> IO ()
substitute location start end text cursor uuid guiID =
    sendRequest $ Message uuid guiID $ Substitute.Request location start end text cursor
