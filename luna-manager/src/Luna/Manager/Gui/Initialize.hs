--Warnig!! temporary file to refactor

module Luna.Manager.Gui.Initialize where

import Prologue hiding (FilePath)
import qualified Data.Map as Map
import Luna.Manager.Component.Repository
import Luna.Manager.Component.Version
import Luna.Manager.System.Host
import Control.Monad.Raise
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
instance FromJSON Initialize   where parseJSON  = lensJSONParse
instance FromJSON Applications where parseJSON  = lensJSONParse
instance FromJSON Apps         where parseJSON  = lensJSONParse



data UnresolvedDepError = UnresolvedDepError deriving (Show)
makeLenses ''UnresolvedDepError

instance Exception UnresolvedDepError where
    displayException err = "Following dependencies were unable to be resolved: " <> show err

unresolvedDepError :: SomeException
unresolvedDepError = toException UnresolvedDepError

getVersionsList :: (MonadIO m, MonadException SomeException m) => Repo -> Text -> m [Version]
getVersionsList repo appName = do
    appPkg <- tryJust unresolvedDepError $ Map.lookup appName $ repo ^. packages
    let vmap   = Map.mapMaybe (Map.lookup currentSysDesc) $ appPkg ^. versions
    return $ sort . Map.keys $ vmap

resolveAppToInitialize :: (MonadIO m, MonadException SomeException m) => Repo -> Text -> m Apps
resolveAppToInitialize repo name = do
    versions <- getVersionsList repo name
    return $ Apps name versions

generateInitialJSON :: (MonadIO m, MonadException SomeException m) => Repo -> m ()
generateInitialJSON repo = do
    resolved <- mapM (resolveAppToInitialize repo) (repo ^. apps)
    print $ encode $ Initialize $ Applications resolved
