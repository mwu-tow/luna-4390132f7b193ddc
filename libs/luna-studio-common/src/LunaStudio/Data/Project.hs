module LunaStudio.Data.Project where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Binary                          (Binary)
import           Data.HashMap.Strict                  (HashMap)
import           Data.IntMap.Lazy                     (IntMap)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.UUID.Types                      (UUID)
import           LunaStudio.Data.Breadcrumb           (Breadcrumb, BreadcrumbItem)
import           LunaStudio.Data.CameraTransformation (CameraTransformation)
import           LunaStudio.Data.Library              (Library)
import           LunaStudio.Data.NodeValue            (Visualizer)
import           LunaStudio.Data.TypeRep              (TypeRep)
import           Prologue                             hiding (TypeRep)




type ProjectId = UUID

data Project = Project { _name     :: String
                       , _libs     :: IntMap Library
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''Project
instance Binary Project



data FileSettings = FileSettings { _lastBreadcrumb        :: Breadcrumb BreadcrumbItem
                                 , _visualizerPreferences :: HashMap TypeRep Visualizer
                                 , _cameraSettings        :: Map (Breadcrumb BreadcrumbItem) CameraTransformation
                                 } deriving (Eq, Generic, Show)

makeLenses ''FileSettings

type ProjectSettings = Map FilePath FileSettings

instance ToJSON   FileSettings
instance FromJSON FileSettings
