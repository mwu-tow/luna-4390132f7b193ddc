{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Internal where

import           Common.Prelude
import           Data.Aeson     (FromJSON, ToJSON)


data InternalEvent = Copy        { _path :: FilePath , _selections :: [(Int, Int)]}
                   | CloseFile   { _path :: FilePath }
                   | FileChanged { _path :: FilePath }
                   | GetBuffer   { _path :: FilePath }
                   | InterpreterPause
                   | InterpreterReload
                   | InterpreterStart
                   | IsSaved     { _path :: FilePath }
                   | OpenFile    { _path :: FilePath }
                   | Paste       { _selections :: [(Int, Int)], _content :: [Text] }
                   | SaveFile    { _path :: FilePath }
                   | SetProject  { _path :: FilePath }
                   deriving (Generic, NFData, Show, Typeable)

makeLenses ''InternalEvent

instance ToJSON   InternalEvent
instance FromJSON InternalEvent
