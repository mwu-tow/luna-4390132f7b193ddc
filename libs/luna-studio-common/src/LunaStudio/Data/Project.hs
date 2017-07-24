module LunaStudio.Data.Project where

import           Control.Lens.Aeson                   (lensJSONParse, lensJSONToEncoding, lensJSONToJSON)
import           Data.Aeson                           (FromJSON (parseJSON), ToJSON (toEncoding, toJSON))
import           Data.Binary                          (Binary)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.IntMap.Lazy                     (IntMap)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.UUID.Types                      (UUID)
import           Data.Yaml                            (decodeFileEither, encodeFile)
import           LunaStudio.Data.Breadcrumb           (Breadcrumb, BreadcrumbItem)
import           LunaStudio.Data.CameraTransformation (CameraTransformation)
import           LunaStudio.Data.GraphLocation        as GraphLocation
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

data ModuleSettings = ModuleSettings { _moduleCurrentBreadcrumb     :: Breadcrumb Text
                                     , _moduleVisualizerPreferences :: HashMap TypeRep Visualizer
                                     , _breadcrumbsSettings         :: Map (Breadcrumb Text) BreadcrumbSettings
                                     } deriving (Eq, Generic, Show)

data ProjectSettings = ProjectSettings { _projectVisualizerPreferences :: HashMap TypeRep Visualizer
                                       , _modulesSettings              :: Map FilePath ModuleSettings
                                       } deriving (Eq, Generic, Show)

makeLenses ''BreadcrumbSettings
makeLenses ''ModuleSettings
makeLenses ''ProjectSettings

instance FromJSON BreadcrumbSettings where parseJSON = lensJSONParse
instance FromJSON ModuleSettings     where parseJSON = lensJSONParse
instance FromJSON ProjectSettings    where parseJSON = lensJSONParse
instance ToJSON   BreadcrumbSettings where
    toJSON     = lensJSONToJSON
    toEncoding = lensJSONToEncoding
instance ToJSON   ModuleSettings     where
    toJSON     = lensJSONToJSON
    toEncoding = lensJSONToEncoding
instance ToJSON   ProjectSettings    where
    toJSON     = lensJSONToJSON
    toEncoding = lensJSONToEncoding

getModuleSettings :: FilePath -> FilePath -> IO (Maybe ModuleSettings)
getModuleSettings configPath modulePath = decodeFileEither configPath >>= \ps -> case ps of
    Left  e               -> print e >> return def
    Right projectSettings -> return $ Map.lookup modulePath projectSettings

updateLocationSettings :: FilePath -> FilePath -> Breadcrumb Text -> HashMap TypeRep Visualizer -> HashMap TypeRep Visualizer -> CameraTransformation -> IO ()
updateLocationSettings configPath filePath bc mvp bcvp camera = decodeFileEither configPath >>= encodeFile configPath . updateProjectSettings where
    createProjectSettings    = ProjectSettings HashMap.empty $ Map.singleton filePath createModuleSettings
    updateProjectSettings    = either (const createProjectSettings) updateModuleSettings
    createModuleSettings     = ModuleSettings bc mvp $ Map.singleton bc createBreadcrumbSettings
    updateModuleSettings'    = ModuleSettings bc mvp . Map.insert bc createBreadcrumbSettings . view breadcrumbsSettings
    updateModuleSettings  ps = ps & modulesSettings . at filePath %~ Just . maybe createModuleSettings updateModuleSettings'
    createBreadcrumbSettings = BreadcrumbSettings bcvp camera
