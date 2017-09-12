{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.Event.Atom where

import           Common.Analytics (IsTrackedEvent)
import           Common.Prelude
import           Data.Aeson       (FromJSON, ToJSON)


data Event = SetFile { path :: FilePath }
           | UnsetFile
           deriving (Eq, FromJSON, Generic, NFData, Show, ToJSON, Typeable)

instance IsTrackedEvent Event
