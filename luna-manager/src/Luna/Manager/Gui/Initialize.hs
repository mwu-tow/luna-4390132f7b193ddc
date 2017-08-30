--Warnig!! temporary file to refactor

module Luna.Manager.Gui.Initialize where

import Prologue hiding (FilePath)
import qualified Data.Map as Map
import Luna.Manager.Component.Repository
import Luna.Manager.Component.Version
import Luna.Manager.System.Host

import Data.Aeson                    (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON, encode)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import qualified Data.ByteString.Lazy as BS
import Control.Lens.Aeson

-- === Definition === --

data Initialize = Initialize { initialize :: Applications
                             } deriving (Show, Generic, Eq)

data Applications = Applications {applications :: [Apps]} deriving (Show, Generic, Eq)

data Apps = Apps { name     :: Text
                 , version :: [Version]} deriving (Show, Generic, Eq)

data Option = Option { install :: Install} deriving (Show, Generic, Eq)

data Install = Install { application :: Text
                       , version     :: Version} deriving (Show, Generic, Eq)

-- makeLenses ''Apps

instance ToJSON Option
instance ToJSON Install
instance ToJSON Initialize
instance ToJSON Applications
instance ToJSON Apps      --  where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON

instance FromJSON Option
instance FromJSON Install
instance FromJSON Initialize where parseJSON  = lensJSONParse
instance FromJSON Applications where parseJSON  = lensJSONParse
instance FromJSON Apps       where parseJSON  = lensJSONParse

getVersionsList :: Repo -> Text -> [Version]
getVersionsList repo appName = sort . Map.keys $ vmap  where
    appPkg = fromMaybe (error "no package for this app") $ Map.lookup appName $ repo ^. packages
    vmap   = Map.mapMaybe (Map.lookup currentSysDesc) $ appPkg ^. versions

resolveAppToInitialize :: Repo -> Text -> Apps
resolveAppToInitialize repo name = Apps name versions where
    versions = getVersionsList repo name

generateInitialJSON :: MonadIO m => Repo -> m ()
generateInitialJSON repo = do
    let resolved = map (resolveAppToInitialize repo) (repo ^. apps)
    print $ encode $ Initialize $ Applications resolved
