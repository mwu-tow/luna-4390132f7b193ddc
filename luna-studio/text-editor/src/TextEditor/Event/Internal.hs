{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Internal where

import           Common.Prelude
import           Data.Aeson     (FromJSON, ToJSON)


data ActionType = CloseFile
                | Copy
                | FileChanged
                | GetBuffer
                | IsSaved
                | OpenFile
                | SaveFile
                | SetProject
                deriving (Bounded, Eq, Enum, Generic, NFData, Read, Show, Typeable)

data InternalEvent = InternalEvent
                   { _dataType   :: ActionType
                   , _path       :: String
                   , _selections :: Maybe [(Int, Int)]
                   } deriving (Generic, NFData, Show, Typeable)

makeLenses ''InternalEvent

instance ToJSON   ActionType
instance FromJSON ActionType
instance ToJSON   InternalEvent
instance FromJSON InternalEvent
