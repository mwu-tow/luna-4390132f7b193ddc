{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Text where

import           Common.Prelude
import           Data.Aeson            (FromJSON, ToJSON)
import           LunaStudio.Data.Point (Point)


data TextEvent = TextEvent
        { _filePath  :: FilePath
        , _start     :: Point
        , _end       :: Point
        , _text      :: Text
        , _cursor    :: Maybe Point
        } deriving (FromJSON, Generic, NFData, Show, ToJSON, Typeable)

makeLenses ''TextEvent
