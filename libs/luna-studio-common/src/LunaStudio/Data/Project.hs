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


data BreadcrumbSettings = BreadcrumbSettings { _breadcrumbVisualizerPreferences :: HashMap TypeRep Visualizer
                                             , _breadcrumbCameraSettings        :: CameraTransformation
                                             } deriving (Eq, Generic, Show)

data ModuleSettings = ModuleSettings { _modulePresentBreadcrumb     :: Breadcrumb BreadcrumbItem
                                     , _moduleVisualizerPreferences :: HashMap TypeRep Visualizer
                                     , _breadcrumbsSettings         :: Map (Breadcrumb BreadcrumbItem) BreadcrumbSettings
                                     } deriving (Eq, Generic, Show)

data ProjectSettings = ProjectSettings { _projectVisualizerPreferences :: HashMap TypeRep Visualizer
                                       , _modulesSettings              :: Map FilePath ModuleSettings
                                       } deriving (Eq, Generic, Show)

makeLenses ''BreadcrumbSettings
makeLenses ''ModuleSettings
makeLenses ''ProjectSettings

instance FromJSON BreadcrumbSettings
instance FromJSON ModuleSettings
instance FromJSON ProjectSettings
instance ToJSON   BreadcrumbSettings
instance ToJSON   ModuleSettings
instance ToJSON   ProjectSettings
